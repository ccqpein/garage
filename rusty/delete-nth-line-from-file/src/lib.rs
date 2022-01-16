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

fn delete_nth_line(f: impl Iterator<Item = Result<String>>, n: &usize) -> Result<Vec<String>> {
    let mut result = vec![];
    for (l_num, ll) in f.enumerate().filter(|(line_num, s)| *line_num + 1 != *n) {
        match ll {
            Ok(s) => result.push(s),
            Err(e) => return Err(e),
        }
    }

    Ok(result)
}

fn delete_nth_lines(f: impl Iterator<Item = Result<String>>, ns: &[usize]) -> Result<Vec<String>> {
    let mut result = vec![];
    for (l_num, ll) in f
        .enumerate()
        .filter(|(line_num, s)| !ns.contains(&(*line_num + 1)))
    {
        match ll {
            Ok(s) => result.push(s),
            Err(e) => return Err(e),
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Result;

    #[test]
    fn test_delete_nth_line() {
        let mut testcase0 = ["1", "2", "3", "4"].iter().map(|a| Ok(a.to_string()));

        let mut testresult0: Vec<String> = ["1", "3", "4"].iter().map(|a| a.to_string()).collect();
        assert_eq!(testresult0, delete_nth_line(testcase0, &2).unwrap());

        let mut testcase1 = ["1", "2", "3", "4"].iter().map(|a| Ok(a.to_string()));
        let mut testresult1: Vec<String> = ["1", "2", "4"].iter().map(|a| a.to_string()).collect();
        assert_eq!(testresult1, delete_nth_line(testcase1, &3).unwrap());
    }

    #[test]
    fn test_delete_nth_lines() {
        let mut testcase0 = ["1", "2", "3", "4"].iter().map(|a| Ok(a.to_string()));

        let mut testresult0: Vec<String> = ["1", "3", "4"].iter().map(|a| a.to_string()).collect();
        assert_eq!(testresult0, delete_nth_lines(testcase0, &[2]).unwrap());

        let mut testcase1 = ["1", "2", "3", "4"].iter().map(|a| Ok(a.to_string()));
        let mut testresult1: Vec<String> = ["1", "4"].iter().map(|a| a.to_string()).collect();
        assert_eq!(testresult1, delete_nth_lines(testcase1, &[2, 3]).unwrap());
    }
}
