use axum::{
    extract::{Form, Path, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse},
    routing::{get, post},
    Router,
};
use notify::{Event, RecommendedWatcher, RecursiveMode, Result as NotifyResult, Watcher};
use pulldown_cmark::{html, Options, Parser};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::Path as StdPath, sync::Arc};
use tokio::sync::{mpsc, RwLock};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ServerConfig {
    pub host: String,
    pub port: u16,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 3000,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BlogConfig {
    pub title: String,
    pub description: String,
    pub author: String,
}

impl Default for BlogConfig {
    fn default() -> Self {
        Self {
            title: "Rust Engine Blog".to_string(),
            description: "A fast, decoupled blog engine powered by Axum and HTMX.".to_string(),
            author: "Anonymous".to_string(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Config {
    #[serde(default)]
    pub server: ServerConfig,
    #[serde(default)]
    pub blog: BlogConfig,
}

impl Config {
    pub fn load() -> Self {
        if StdPath::new("config.toml").exists() {
            match std::fs::read_to_string("config.toml") {
                Ok(content) => match toml::from_str(&content) {
                    Ok(cfg) => {
                        tracing::info!("Loaded configuration from config.toml");
                        return cfg;
                    }
                    Err(e) => {
                        tracing::error!("Failed to parse config.toml: {}", e);
                    }
                },
                Err(e) => {
                    tracing::error!("Failed to read config.toml: {}", e);
                }
            }
        } else {
            tracing::warn!("config.toml not found, using default configuration");
        }
        Config::default()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PostMeta {
    pub title: String,
    pub date: String,
    pub slug: String,
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(default)]
    pub summary: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Post {
    pub meta: PostMeta,
    pub content_html: String,
    pub markdown_body: String,
}

pub type PostIndex = HashMap<String, Post>;

#[derive(Clone)]
pub struct AppState {
    pub posts: Arc<RwLock<PostIndex>>,
    pub templates: Arc<RwLock<minijinja::Environment<'static>>>,
    pub config: Config,
}

fn slugify(text: &str) -> String {
    let mut slug = String::with_capacity(text.len());
    let mut prev_is_dash = true; // avoid leading dash

    for ch in text.chars() {
        if ch.is_alphanumeric() {
            for lower in ch.to_lowercase() {
                slug.push(lower);
            }
            prev_is_dash = false;
        } else if ch == '-' || ch == '_' || ch.is_whitespace() {
            if !prev_is_dash {
                slug.push('-');
                prev_is_dash = true;
            }
        }
    }

    if slug.ends_with('-') {
        slug.pop();
    }

    if slug.is_empty() {
        "section".to_string()
    } else {
        slug
    }
}

fn markdown_to_html(markdown: &str) -> String {
    use pulldown_cmark::{Event, HeadingLevel, Tag, TagEnd};

    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);
    options.insert(Options::ENABLE_FOOTNOTES);
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TASKLISTS);
    options.insert(Options::ENABLE_HEADING_ATTRIBUTES);

    let parser = Parser::new_ext(markdown, options);
    let mut events = parser.into_iter();
    let mut transformed_events: Vec<Event> = Vec::new();
    let mut slug_counts: HashMap<String, usize> = HashMap::new();

    while let Some(event) = events.next() {
        match event {
            Event::Start(Tag::Heading {
                level,
                id,
                classes: _,
                attrs: _,
            }) => {
                let level_num = match level {
                    HeadingLevel::H1 => 1,
                    HeadingLevel::H2 => 2,
                    HeadingLevel::H3 => 3,
                    HeadingLevel::H4 => 4,
                    HeadingLevel::H5 => 5,
                    HeadingLevel::H6 => 6,
                };

                let mut inner_events = Vec::new();
                let mut plain_text = String::new();

                for inner_ev in events.by_ref() {
                    if let Event::End(TagEnd::Heading(_)) = inner_ev {
                        break;
                    }
                    match &inner_ev {
                        Event::Text(t) => plain_text.push_str(t),
                        Event::Code(c) => plain_text.push_str(c),
                        _ => {}
                    }
                    inner_events.push(inner_ev);
                }

                let raw_slug = if let Some(explicit_id) = id {
                    explicit_id.to_string()
                } else {
                    slugify(&plain_text)
                };

                let count = slug_counts.entry(raw_slug.clone()).or_insert(0);
                let unique_slug = if *count == 0 {
                    raw_slug.clone()
                } else {
                    format!("{}-{}", raw_slug, count)
                };
                *count += 1;

                let mut inner_html = String::new();
                html::push_html(&mut inner_html, inner_events.into_iter());

                let heading_html = format!(
                    "<h{lvl} id=\"{slug}\" class=\"group relative flex items-center\">\
                        <a href=\"#{slug}\" class=\"heading-permalink no-underline text-inherit hover:text-indigo-300 transition-colors inline-flex items-center gap-2\">\
                            <span>{inner}</span>\
                            <span class=\"opacity-0 group-hover:opacity-100 text-indigo-400 font-mono text-sm transition-opacity select-none\" aria-hidden=\"true\">#</span>\
                        </a>\
                    </h{lvl}>\n",
                    lvl = level_num,
                    slug = unique_slug,
                    inner = inner_html.trim()
                );

                transformed_events.push(Event::Html(heading_html.into()));
            }
            other => {
                transformed_events.push(other);
            }
        }
    }

    let mut html_output = String::new();
    html::push_html(&mut html_output, transformed_events.into_iter());
    html_output
}

fn parse_markdown_file(content: &str, file_path: &StdPath) -> Option<Post> {
    let content = content.trim_start();
    if !content.starts_with("---") {
        tracing::warn!("File {:?} missing frontmatter opening '---'", file_path);
        return None;
    }

    let rest = &content[3..];
    let end_idx = match rest.find("---") {
        Some(idx) => idx,
        None => {
            tracing::warn!("File {:?} missing frontmatter closing '---'", file_path);
            return None;
        }
    };

    let yaml_str = &rest[..end_idx];
    let markdown_body = rest[end_idx + 3..].trim();

    let meta: PostMeta = match serde_yaml::from_str(yaml_str) {
        Ok(m) => m,
        Err(e) => {
            tracing::error!("Failed to parse YAML frontmatter in {:?}: {}", file_path, e);
            return None;
        }
    };

    let content_html = markdown_to_html(markdown_body);

    Some(Post {
        meta,
        content_html,
        markdown_body: markdown_body.to_string(),
    })
}

pub fn scan_posts_dir(dir_path: &str) -> PostIndex {
    let mut index = HashMap::new();
    let entries = match std::fs::read_dir(dir_path) {
        Ok(entries) => entries,
        Err(e) => {
            tracing::error!("Failed to read content directory '{}': {}", dir_path, e);
            return index;
        }
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "md") {
            match std::fs::read_to_string(&path) {
                Ok(content) => {
                    if let Some(post) = parse_markdown_file(&content, &path) {
                        index.insert(post.meta.slug.clone(), post);
                    }
                }
                Err(e) => {
                    tracing::error!("Failed to read post file {:?}: {}", path, e);
                }
            }
        }
    }
    tracing::info!("Scanned {} valid post(s) into PostIndex", index.len());
    index
}

pub fn create_template_env() -> minijinja::Environment<'static> {
    let mut env = minijinja::Environment::new();
    env.set_loader(minijinja::path_loader("./templates"));
    env
}

pub fn setup_watcher(state: AppState) -> Result<RecommendedWatcher, Box<dyn std::error::Error>> {
    let (tx, mut rx) = mpsc::channel::<()>(100);

    let mut watcher = notify::RecommendedWatcher::new(
        move |res: NotifyResult<Event>| {
            if let Ok(event) = res {
                if event.kind.is_create() || event.kind.is_modify() || event.kind.is_remove() {
                    let _ = tx.blocking_send(());
                }
            }
        },
        notify::Config::default(),
    )?;

    if StdPath::new("./content").exists() {
        watcher.watch(StdPath::new("./content"), RecursiveMode::Recursive)?;
    }
    if StdPath::new("./templates").exists() {
        watcher.watch(StdPath::new("./templates"), RecursiveMode::Recursive)?;
    }

    tokio::spawn(async move {
        while rx.recv().await.is_some() {
            // Short debounce to allow multi-file write operations to settle
            tokio::time::sleep(tokio::time::Duration::from_millis(20)).await;
            while rx.try_recv().is_ok() {}

            tracing::info!("File system event detected: updating PostIndex and MiniJinja Environment");
            let new_posts = scan_posts_dir("./content");
            {
                let mut guard = state.posts.write().await;
                *guard = new_posts;
            }

            let new_env = create_template_env();
            {
                let mut guard = state.templates.write().await;
                *guard = new_env;
            }
        }
    });

    Ok(watcher)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "blog_server=info,tower_http=info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    tracing::info!("Initializing blog-server engine...");

    let config = Config::load();
    let posts = scan_posts_dir("./content");
    let templates = create_template_env();

    let state = AppState {
        posts: Arc::new(RwLock::new(posts)),
        templates: Arc::new(RwLock::new(templates)),
        config,
    };

    let _watcher = setup_watcher(state.clone())?;

    let static_service = tower_http::services::ServeDir::new("./static");

    let app = Router::new()
        .route("/", get(index_handler))
        .route("/posts/:slug", get(post_handler))
        .route("/api/search", post(search_handler))
        .nest_service("/static", static_service)
        .layer(tower_http::trace::TraceLayer::new_for_http())
        .with_state(state.clone());

    let addr = format!("{}:{}", state.config.server.host, state.config.server.port);
    let listener = tokio::net::TcpListener::bind(&addr).await?;
    tracing::info!("Server listening on http://{}", addr);

    axum::serve(listener, app).await?;

    Ok(())
}

async fn index_handler(State(state): State<AppState>) -> impl IntoResponse {
    let posts_guard = state.posts.read().await;
    let mut posts: Vec<Post> = posts_guard.values().cloned().collect();
    posts.sort_by(|a, b| b.meta.date.cmp(&a.meta.date));
    drop(posts_guard);

    let env_guard = state.templates.read().await;
    let tmpl = match env_guard.get_template("index.html") {
        Ok(t) => t,
        Err(e) => {
            tracing::error!("Failed to get template 'index.html': {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Template Error</h1>".to_string()),
            );
        }
    };

    let rendered = match tmpl.render(minijinja::context! {
        config => &state.config,
        posts => &posts,
    }) {
        Ok(html) => html,
        Err(e) => {
            tracing::error!("Failed to render 'index.html': {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Render Error</h1>".to_string()),
            );
        }
    };

    (StatusCode::OK, Html(rendered))
}

async fn post_handler(
    State(state): State<AppState>,
    Path(slug): Path<String>,
) -> impl IntoResponse {
    let posts_guard = state.posts.read().await;
    let post = match posts_guard.get(&slug) {
        Some(p) => p.clone(),
        None => {
            tracing::warn!("Post not found for slug: {}", slug);
            return (
                StatusCode::NOT_FOUND,
                Html("<h1 style='color: white; padding: 2rem;'>404 Post Not Found</h1>".to_string()),
            );
        }
    };
    drop(posts_guard);

    let env_guard = state.templates.read().await;
    let tmpl = match env_guard.get_template("post.html") {
        Ok(t) => t,
        Err(e) => {
            tracing::error!("Failed to get template 'post.html': {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Template Error</h1>".to_string()),
            );
        }
    };

    let rendered = match tmpl.render(minijinja::context! {
        config => &state.config,
        post => &post,
    }) {
        Ok(html) => html,
        Err(e) => {
            tracing::error!("Failed to render 'post.html': {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Render Error</h1>".to_string()),
            );
        }
    };

    (StatusCode::OK, Html(rendered))
}

#[derive(Debug, Deserialize)]
pub struct SearchForm {
    pub q: Option<String>,
}

async fn search_handler(
    State(state): State<AppState>,
    headers: HeaderMap,
    Form(form): Form<SearchForm>,
) -> impl IntoResponse {
    let query = form.q.unwrap_or_default().trim().to_lowercase();

    let posts_guard = state.posts.read().await;
    let mut posts: Vec<Post> = posts_guard
        .values()
        .filter(|p| {
            if query.is_empty() {
                true
            } else {
                p.meta.title.to_lowercase().contains(&query)
                    || p.meta.summary.as_ref().is_some_and(|s| s.to_lowercase().contains(&query))
                    || p.meta.tags.iter().any(|t| t.to_lowercase().contains(&query))
            }
        })
        .cloned()
        .collect();

    posts.sort_by(|a, b| b.meta.date.cmp(&a.meta.date));
    drop(posts_guard);

    let is_hx_request = headers.get("HX-Request").is_some_and(|v| v == "true");

    let env_guard = state.templates.read().await;
    let template_name = if is_hx_request {
        "partials/search_results.html"
    } else {
        "index.html"
    };

    let tmpl = match env_guard.get_template(template_name) {
        Ok(t) => t,
        Err(e) => {
            tracing::error!("Failed to get search template '{}': {}", template_name, e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Template Error</h1>".to_string()),
            );
        }
    };

    let rendered = match tmpl.render(minijinja::context! {
        config => &state.config,
        posts => &posts,
        query => &query,
    }) {
        Ok(html) => html,
        Err(e) => {
            tracing::error!("Failed to render search template: {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Html("<h1>Render Error</h1>".to_string()),
            );
        }
    };

    (StatusCode::OK, Html(rendered))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slugify() {
        assert_eq!(slugify("Design"), "design");
        assert_eq!(slugify("What's Next?"), "whats-next");
        assert_eq!(slugify("Day 1 & 2 & 3 & 4"), "day-1-2-3-4");
        assert_eq!(
            slugify("What did I do with lazy_static?"),
            "what-did-i-do-with-lazy-static"
        );
        assert_eq!(slugify("   --- Hello   World! ---  "), "hello-world");
        assert_eq!(slugify("!!!"), "section");
    }

    #[test]
    fn test_markdown_to_html_headings() {
        let md = "# Main Title\n\nSome text.\n\n## Section 1\n\nContent here.\n\n## Section 1\n\nDuplicate section.\n\n### Sub `Code` Heading";
        let html = markdown_to_html(md);

        assert!(html.contains("<h1 id=\"main-title\" class=\"group relative flex items-center\"><a href=\"#main-title\" class=\"heading-permalink no-underline text-inherit hover:text-indigo-300 transition-colors inline-flex items-center gap-2\"><span>Main Title</span><span class=\"opacity-0 group-hover:opacity-100 text-indigo-400 font-mono text-sm transition-opacity select-none\" aria-hidden=\"true\">#</span></a></h1>"));
        assert!(html.contains("id=\"section-1\""));
        assert!(html.contains("id=\"section-1-1\""));
        assert!(html.contains("id=\"sub-code-heading\""));
        assert!(html.contains("<code>Code</code>"));
    }

    #[test]
    fn test_scan_all_content_posts() {
        let posts = scan_posts_dir("./content");
        assert!(!posts.is_empty(), "Posts index should not be empty");
        for (slug, post) in posts {
            assert!(!post.content_html.is_empty(), "Post {} html should not be empty", slug);
        }
    }
}

