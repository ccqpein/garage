use lazy_static::lazy_static;
use std::{fs::File, io::Read, path::Path};

use actix_web::{
    dev::{HttpServiceFactory, ServiceFactory, ServiceRequest},
    get,
    http::header::ContentType,
    web, App, Error, FromRequest, Handler, HttpResponse, Responder, Scope,
};

lazy_static! {
    static ref LAST_RESUME: Resume<'static> = Resume::default();
    static ref RESUME_HTML: &'static str = "empty";
}

#[derive(Clone, Default)]
pub struct Resume<'r> {
    /// page url
    page_url: &'r str,

    /// page html path in filesystem
    page_name: &'r str,

    /// pdf path in filesystem
    pdf: &'r str,
}

impl<'r> Resume<'r> {
    fn new(page_url: &'r str, page_name: &'r str, pdf: &'r str) -> Self {
        Self {
            page_url,
            page_name,
            pdf,
        }
    }

    /// read the config of resume app
    pub fn from_file_config(path: impl AsRef<Path>) -> Self {
        todo!()
    }

    /// read the resume html
    fn resume(&self) -> std::io::Result<String> {
        let mut file = File::open(self.page_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }

    /// update the resume struct in case the file updated on disk
    fn update(&mut self) -> std::io::Result<()> {
        Ok(())
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
    *RESUME_HTML
}

async fn handler_dl(path: web::Path<String>) -> impl Responder {
    *RESUME_HTML
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
