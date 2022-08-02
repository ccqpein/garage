use actix_web::{
    dev::{HttpServiceFactory, ServiceFactory, ServiceRequest},
    get,
    http::header::ContentType,
    web, App, Error, FromRequest, Handler, HttpResponse, Responder, Scope,
};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};
use tokio::sync::Mutex;
use tracing::debug;

lazy_static! {
    static ref LAST_RESUME: Mutex<Resume> = Mutex::new(Resume::default());
    static ref RESUME_HTML: Mutex<String> = Mutex::new(String::new());
}

#[derive(Clone, Default, Serialize, Deserialize, Debug)]
pub struct Resume {
    /// page url
    page_url: String,

    /// page html path in filesystem
    page_name: String,

    /// pdf path in filesystem
    pdf: String,
}

impl Resume {
    fn new(page_url: String, page_name: String, pdf: String) -> Self {
        Self {
            page_url,
            page_name,
            pdf,
        }
    }

    /// read the resume html
    fn resume(&self) -> std::io::Result<String> {
        let mut file = File::open(&self.page_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }

    /// update the resume struct in case the file updated on disk
    /// update the static LAST_RESUME
    async fn update(conf_path: impl AsRef<Path>) -> std::io::Result<()> {
        let resume = Self::from_file_config(conf_path).await?;

        // update the html
        // write like this way can avoid deadlock
        // release locker after below line
        *RESUME_HTML.lock().await = resume.resume()?;

        // update the resume instance
        *LAST_RESUME.lock().await = resume;

        Ok(())
    }

    /// read the config of resume app
    pub async fn from_file_config(path: impl AsRef<Path>) -> std::io::Result<Self> {
        println!("here");
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let r: Self = serde_json::from_reader(reader)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;
        *LAST_RESUME.lock().await = r.clone();
        *RESUME_HTML.lock().await = r.resume()?;
        debug!("read resume config: {:?}", &r);
        Ok(r)
    }

    pub fn register_resume_service<T>(&self, app: App<T>) -> App<T>
    where
        T: ServiceFactory<ServiceRequest, Config = (), Error = Error, InitError = ()>,
    {
        app.service(
            web::scope("/resume")
                .route("/{page_url}", web::get().to(handler_html))
                .route("/{page_url}/dl", web::get().to(handler_dl)),
        )
    }
}

async fn handler_html(path: web::Path<String>) -> impl Responder {
    debug!("handler_html path is {}", path);
    if LAST_RESUME.lock().await.page_url == path.into_inner() {
        HttpResponse::Ok()
            .content_type(ContentType::html())
            .body(RESUME_HTML.lock().await.clone())
    } else {
        HttpResponse::NotFound().finish()
    }
}

async fn handler_dl(path: web::Path<String>) -> impl Responder {
    //:= download pdf
    HttpResponse::Ok()
}

#[cfg(test)]
mod tests {
    use actix_web::{body::MessageBody, dev::ServiceResponse, http::StatusCode, test, web::Bytes};

    use super::*;

    #[actix_web::test]
    async fn test_handler_path_match() {
        let app = test::init_service(
            App::new().service(
                web::scope("/resume")
                    .route("/{page_url}", web::get().to(handler_html))
                    .route("/{page_url}/dl", web::get().to(handler_dl)),
            ),
        )
        .await;

        let req = test::TestRequest::default()
            .uri("/resume/urlurlu")
            .to_request();

        let res = test::call_service(&app, req).await;
        // dbg!(res.response().body());
        // let (_, resp) = res.into_parts();
        // assert_eq!(
        //     Bytes::from("empty"),
        //     resp.into_body().try_into_bytes().unwrap()
        // );
        assert_eq!(res.status(), StatusCode::OK);

        //////////////////

        let req = test::TestRequest::default()
            .uri("/resume/urlurlu/dl")
            .to_request();
        // let res = test::call_service(&app, req).await;
        // dbg!(res.response().body());
        // let (_, resp) = res.into_parts();
        // assert_eq!(
        //     Bytes::from("empty"),
        //     resp.into_body().try_into_bytes().unwrap()
        // );
        assert_eq!(res.status(), StatusCode::OK);

        //////////////////////

        let req = test::TestRequest::default()
            .uri("/resume/urlurlu/dlaaa")
            .to_request();
        let res = test::call_service(&app, req).await;
        //dbg!(req.match_info());
        //dbg!(req.match_pattern());
        assert_eq!(res.status(), StatusCode::NOT_FOUND)
    }
}
