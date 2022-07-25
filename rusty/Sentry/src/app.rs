use std::path::Path;

use actix_web::{dev::HttpServiceFactory, web, HttpResponse};

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

    fn into_service(self) -> impl HttpServiceFactory + 'static {
        web::scope(self.page_url)
            .service(web::resource("/dl").to(|| HttpResponse::Ok()))
            .default_service(web::to(|| {
                //:= todo: return html content
                HttpResponse::NotFound()
            }))
    }
}
