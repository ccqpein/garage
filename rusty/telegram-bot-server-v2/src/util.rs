use std::{
    collections::HashSet,
    env,
    ffi::OsString,
    path::{Path, PathBuf},
};

/// util for app to use to handle the message
use telegram_bot::{Api, File, GetFile, ToFileRef};
use tracing::debug;
//use std::fs::File;
use std::io::Write;

pub struct FileDownloader {
    api: Api,
    token: String,
    reqwest_client: reqwest::Client,
    folder: String,
}

impl FileDownloader {
    pub fn new(token: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let api = Api::new(token);
        let current_path = env::current_exe()?;
        let parent_path = current_path
            .parent()
            .ok_or::<String>("cannot get the parant path".into())?;

        let mut path = PathBuf::new();
        path.push(parent_path);
        path.push("download_files");

        debug!("download path: {:?}", path.as_path().to_str());

        Ok(Self {
            api,
            token: token.to_string(),
            reqwest_client: reqwest::Client::new(),

            folder: path
                .as_path()
                .to_str()
                .ok_or::<String>("cannot get the parant path".into())?
                .to_string(),
        })
    }

    pub async fn download_file(
        &self,
        file_ref: impl ToFileRef,
    ) -> Result<String, Box<dyn std::error::Error>> {
        match self
            .api
            .send(GetFile::new(file_ref))
            .await
            .map_err(|e| e.to_string())
        {
            Ok(File {
                file_id, file_path, ..
            }) => {
                let path = self.folder.clone() + "/" + &file_id;
                if Path::new(&path).exists() {
                    return Ok(path);
                }

                let mut f = std::fs::File::create(path.clone())?;
                let f_url = "https://api.telegram.org/file/bot".to_string()
                    + &self.token
                    + "/"
                    + file_path
                        .ok_or::<String>("file path is nil".into())?
                        .as_str();

                let r = self.reqwest_client.get(f_url).send().await?;

                // write file
                f.write_all(&r.bytes().await?)?;

                debug!("downloaded file path is {}", path);
                Ok(path)
            }
            Err(e) => Err(e.to_string().into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_current_exe_path() -> Result<(), Box<dyn std::error::Error>> {
        let current_path = env::current_exe()?;
        let parent_path = current_path
            .parent()
            .ok_or::<String>("cannot get the parant path".into())?;

        let mut path = PathBuf::new();
        path.push(parent_path);
        path.push("download_files");

        dbg!(current_path);
        dbg!(path.as_path().to_str());
        dbg!(path.as_path().to_str().unwrap().to_string() + "/" + "aaa");

        Ok(())
    }
}
