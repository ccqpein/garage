use std::{
    env,
    path::{Path, PathBuf},
};

/// util for app to use to handle the message
use telegram_bot::{Api, File, GetFile, ToFileRef};
use tracing::debug;
//use std::fs::File;
use std::io::Write;

struct FileDownloader {
    api: Api,
    reqwest_client: reqwest::Client,
    folder: String,
}

impl FileDownloader {
    fn new(api: Api) -> Result<Self, Box<dyn std::error::Error>> {
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
            reqwest_client: reqwest::Client::new(),
            folder: path
                .as_path()
                .to_str()
                .ok_or::<String>("cannot get the parant path".into())?
                .to_string(),
        })
    }

    //:= need to get the file type, like "mime_type: Some("audio/ogg")"
    //:= audio/ogg need to transfer mp3? or not?
    async fn download_file(
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
                let mut f = std::fs::File::create(path.clone())?;
                let f_url = file_path.ok_or::<String>("file path is nil".into())?;
                let r = self.reqwest_client.get(f_url).send().await?;

                // write file
                f.write_all(&r.bytes().await?)?;

                //:= TODO: need to cache if the file downloaded or not

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
