use std::{
    fs::File,
    io::{self, BufRead, BufReader, Result},
    path::Path,
};

fn read_file(f: impl AsRef<Path>) -> Result<impl Iterator<Item = Result<String>>> {
    let f = File::open(f)?;
    let mut reader = BufReader::new(f);
    Ok(reader.lines())
}

fn delete_nth_line(f: impl Iterator<Item = Result<String>>, n: usize) -> Result<Vec<String>> {
    let mut line_mark = 0;

    let mut result = vec![];
    for (l_num, ll) in f.enumerate().filter(|(line_num, s)| *line_num != n) {
        match ll {
            Ok(s) => result.push(s),
            Err(e) => return Err(e),
        }
    }

    Ok(result)
}
