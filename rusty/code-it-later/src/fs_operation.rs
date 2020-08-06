use super::config::Config;
use std::fs::read_dir;
use std::fs::*;
use std::io::prelude::*;
use std::io::Result;
use std::path::Path;
use std::path::PathBuf;
use std::sync::mpsc::{Receiver, Sender};

//:= TODO: maybe stackoverflow
pub fn all_files_in_dir<T: AsRef<Path>>(
    p: T,
    conf: &Config,
    ch: Sender<Vec<PathBuf>>,
) -> Result<()> {
    let (files, dirs) = files_and_dirs_in_path(p, conf)?;
    ch.send(files).unwrap_or_else(|e| println!("{}", e));

    if dirs.len() != 0 {
        dirs.iter()
            .map(|d| (d, ch.clone()))
            .for_each(|(d, ch)| all_files_in_dir(d, conf, ch).unwrap())
    }

    Ok(())
}

type Dirs = Vec<PathBuf>;
type Files = Vec<PathBuf>;

fn files_and_dirs_in_path(p: impl AsRef<Path>, conf: &Config) -> Result<(Files, Dirs)> {
    let (mut f, mut d): (Files, Dirs) = (vec![], vec![]);

    // get filetypes
    let filetypes = &conf.filetypes;
    let filetypes_count = filetypes.len();

    // get ignore dirs
    let ignore_dirs = &conf.ignore_dirs;
    let ignore_dirs_count = ignore_dirs.len();

    for entry in read_dir(p)? {
        let dir = entry?;
        let path = dir.path();

        if path.is_dir() {
            // check ignore dirs
            if ignore_dirs_count != 0 {
                if let Some(d_name) = path.file_name() {
                    if !ignore_dirs.contains(&d_name.to_os_string()) {
                        d.push(path)
                    }
                }
            } else {
                d.push(path)
            }
        } else {
            // check filetypes
            if filetypes_count != 0 {
                if let Some(t) = path.extension() {
                    if filetypes.contains(&t.to_os_string()) {
                        f.push(path)
                    }
                }
            } else {
                f.push(path)
            }
        }
    }
    Ok((f, d))
}

struct Crumb {
    line_num: usize,
    content: String,
}

//:= TODO: need regex here
fn filter_line(line: &str, line_num: usize) -> Option<Crumb> {
    None
}

fn op_file(file: PathBuf, conf: &Config) -> Result<Vec<Crumb>> {
    let mut buf = vec![];
    let mut f: File = File::open(file)?;
    f.read_to_end(&mut buf)?;

    let mut line_num = 0;
    let mut ss = String::new();
    let mut buf = buf.as_slice();
    let mut result = vec![];
    loop {
        line_num += 1;
        match buf.read_line(&mut ss) {
            Ok(0) | Err(_) => break,
            Ok(_) => match filter_line(&ss, line_num) {
                Some(cb) => result.push(cb),
                None => (),
            },
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_files_and_dirs_in_path() -> Result<()> {
        let (fs, dirs) = files_and_dirs_in_path("./tests", &Default::default())?;

        assert_eq!(dirs.len(), 0);
        assert_eq!(fs, vec![PathBuf::from("./tests/test.py"),]);
        Ok(())
    }
}
