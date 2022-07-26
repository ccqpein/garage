use std::{fs::File, io::Read, path::Path};

use actix_web::{
    dev::HttpServiceFactory, http::header::ContentType, web, FromRequest, Handler, HttpResponse,
    Responder,
};

#[derive(Clone)]
struct Resume<'r> {
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
    fn from_file_config(path: impl AsRef<Path>) {
        todo!()
    }

    /// read the resume html
    fn resume(&self) -> std::io::Result<String> {
        let mut file = File::open(self.page_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }

    // async fn into_response(self) -> std::io::Result<String> {
    //     // HttpResponse::Ok()
    //     //     .content_type(ContentType::html())
    //     //     .body(self.resume().unwrap())
    //     todo!()
    // }

    fn into_response<F, Args>() -> F
    where
        F: Handler<Args>,
        Args: FromRequest + 'static,
        F::Output: Responder + 'static,
    {
        todo!()
    }

    async fn into_service(self) -> impl HttpServiceFactory + 'static {
        //let resume = self.resume().unwarp();
        web::scope(self.page_url.clone())
            .service(web::resource("/dl").to(|| HttpResponse::Ok()))
            // .default_service(web::to(|| {
            //     HttpResponse::Ok()
            //         .content_type(ContentType::html())
            //         .body(resume)
            // }))
            //.default_service(web::to(self.into_response().await))
            .service(web::resource("").to(Self::into_response()))
    }
}

async fn index() -> std::io::Result<String> {
    todo!()
}
