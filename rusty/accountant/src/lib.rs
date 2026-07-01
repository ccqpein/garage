use accountant_rpc::Transaction;
use std::path::PathBuf;

pub mod fs;

/// Entry function that receives a PathBuf and string.
/// Write the content to file
pub fn entry(path: PathBuf, tx: &Transaction) -> anyhow::Result<()> {
    // 1. check the transaction timestamp and pick the year out
    let parsed_zdt = tx.timestamp.parse::<jiff::Zoned>()?;
    let year = parsed_zdt.year();

    // 2. find the {year}.lisp file in path folder, using ensure_directory_exists and ensure_file_exists
    crate::fs::ensure_directory_exists(&path)?;
    let filename = format!("{}.lisp", year);
    let file_path = path.join(&filename);
    crate::fs::ensure_file_exists(&file_path)?;

    // 3. insert the tx.serialize_lisp() to file
    use lisp_rpc_rust_server::ToRPCType;
    let lisp_str = tx.serialize_lisp()?;
    let content_to_append = format!("{}\n", lisp_str);
    crate::fs::append_string_to_file(&content_to_append, path, &filename)?;

    Ok(())
}

/// Gets the timestamp in the specified timezone.
///
/// If `time` is `None`, the current system time (`now`) is used.
/// If `timezone` is `None`, `UTC` is used.
pub fn get_timestamp(
    timezone: Option<String>,
    time: Option<jiff::civil::DateTime>,
) -> anyhow::Result<String> {
    let tz_str = timezone.as_deref().unwrap_or("UTC");
    let zoned = match time {
        Some(civil_dt) => civil_dt.in_tz(tz_str)?,
        None => jiff::Zoned::now().in_tz(tz_str)?,
    };

    Ok(zoned.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use jiff::civil::date;

    #[test]
    fn test_get_timestamp_utc_now() {
        let ts_str = get_timestamp(None, None).unwrap();
        // Should contain date/time separator 'T'
        assert!(ts_str.contains('T'));
    }

    #[test]
    fn test_get_timestamp_with_timezone_and_time() {
        // 2024-07-15 21:27:00
        let dt = date(2024, 7, 15).at(21, 27, 0, 0);
        let ts_str = get_timestamp(Some("America/New_York".to_string()), Some(dt)).unwrap();

        // Assert New York is represented with its offset and RFC 9557 brackets
        assert_eq!(ts_str, "2024-07-15T21:27:00-04:00[America/New_York]");

        // Parse the same local time in UTC to compare
        let ts_utc_str = get_timestamp(None, Some(dt)).unwrap();
        assert_eq!(ts_utc_str, "2024-07-15T21:27:00+00:00[UTC]");
    }

    #[test]
    fn test_entry_success() {
        let test_dir = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("test_entry_dir");
        if test_dir.exists() {
            std::fs::remove_dir_all(&test_dir).unwrap();
        }

        let tx_json = r#"{
            "timestamp": "2024-07-15T21:27:00-04:00[America/New_York]",
            "account": "Assets:Checking",
            "tx_type": "debit",
            "amount": 1000,
            "category": ["Food", "Groceries"]
        }"#;
        let tx: Transaction = serde_json::from_str(tx_json).unwrap();

        entry(test_dir.clone(), &tx).unwrap();

        let lisp_file = test_dir.join("2024.lisp");
        assert!(lisp_file.is_file());

        let content = std::fs::read_to_string(lisp_file).unwrap();
        assert!(content.contains("transaction"));
        assert!(content.contains("Assets:Checking"));

        std::fs::remove_dir_all(&test_dir).unwrap();
    }
}
