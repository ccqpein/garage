use reqwest::blocking::Client;
use reqwest::blocking::Request;
use reqwest::blocking::{get, Response};
use reqwest::cookie::Jar;
use reqwest::Method;
use reqwest::{IntoUrl, Url};

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::sync::Arc;

/// return response
pub fn get_quiz_by_url(url: &str) -> Result<Response, String> {
    let token = get_csrftoken("./token")?; //:= need change to some other path
    let cli = make_client(&token, url).map_err(|e| e.to_string())?;
    let req = request_builder(&cli, Method::GET, url).map_err(|e| e.to_string())?;
    cli.execute(req).map_err(|e| e.to_string())
}

/// read csrftoken from file
fn get_csrftoken(path: impl AsRef<Path>) -> Result<String, String> {
    let mut f = File::open(path).map_err(|e| e.to_string())?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).map_err(|e| e.to_string())?;
    String::from_utf8(buffer).map_err(|e| e.to_string())
}

fn make_client(cookie: &str, url: impl IntoUrl) -> reqwest::Result<Client> {
    let jar = Jar::default();
    jar.add_cookie_str(cookie, &url.into_url()?);
    Client::builder()
        .cookie_store(true)
        .cookie_provider(Arc::new(jar))
        .build()
}

fn request_builder(
    client: &Client,
    method: Method,
    url: impl IntoUrl + Clone,
) -> reqwest::Result<Request> {
    let url_str = url.clone();
    client
        .request(method, url)
        .header(
            "X-CSRFToken",
            "c6J80TGFMuNrdy4O9cuedV9fc9k0WURzxRUmTqkcgUs9sYbfJi8ZUKuR8iqVmB6t",
        )
        .header("referer", url_str.as_str())
        .header("Content-Type", "application/json")
        .build()
}
