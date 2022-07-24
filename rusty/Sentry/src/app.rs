use std::path::Path;

struct Resume<'r> {
    page_url: &'r str,
    page_name: &'r str,

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

    fn from_file_config(path: impl AsRef<Path>) {
        todo!()
    }
}
