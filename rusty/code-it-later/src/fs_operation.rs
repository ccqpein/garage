use std::fs::read_dir;
use std::io::Result;
use std::path::Path;
use std::path::PathBuf;

use std::sync::mpsc::{Receiver, Sender};
struct FsOpConfig {
    filetype: Option<Vec<String>>,
}

fn all_files_in_dir<T: AsRef<Path> + Sized>(
    p: T,
    conf: &FsOpConfig,
    ch: Sender<Vec<PathBuf>>,
) -> Result<()> {
    let (files, dirs) = files_and_dirs_in_path(p)?;
    ch.send(files);

    if dirs.len() != 0 {
        dirs.iter()
            .map(|d| (d, ch.clone()))
            .for_each(|(d, ch)| all_files_in_dir(d, conf, ch).unwrap())
    }

    Ok(())
}

type Dirs = Vec<PathBuf>;
type Files = Vec<PathBuf>;

fn files_and_dirs_in_path(p: impl AsRef<Path>) -> Result<(Files, Dirs)> {
    let (mut f, mut d): (Files, Dirs) = (vec![], vec![]);
    for entry in read_dir(p)? {
        let dir = entry?;
        let path = dir.path();
        if path.is_dir() {
            d.push(path)
        } else {
            f.push(path)
        }
    }
    Ok((f, d))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_files_and_dirs_in_path() -> Result<()> {
        let (fs, dirs) = files_and_dirs_in_path("./tests")?;

        assert_eq!(dirs.len(), 0);
        assert_eq!(fs, vec![PathBuf::from("./tests/test.py"),]);
        Ok(())
    }
}
