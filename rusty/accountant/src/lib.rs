#![feature(once_cell_get_mut)]

use accountant_rpc::Transaction;
use lisp_rpc_rust_server::{ToRPCType, lisp_rpc_to_str};
use std::{
    path::PathBuf,
    sync::{Mutex, OnceLock},
};

pub mod account;
use account::*;

pub mod fs;

static ACCOUNTS: OnceLock<Mutex<Vec<Account>>> = OnceLock::new();

/// Entry function that receives a PathBuf and string.
/// Write the content to file
pub fn entry(path: PathBuf, tx: &mut Transaction) -> anyhow::Result<()> {
    let accs = match ACCOUNTS.get() {
        Some(x) => x,
        None => {
            let accounts = load_accounts(path.clone())?;
            ACCOUNTS.get_or_init(|| Mutex::new(accounts))
        }
    };

    // 1. check the transaction timestamp and pick the year out
    let parsed_dt = tx.timestamp.parse::<jiff::civil::DateTime>()?;
    let year = parsed_dt.year();

    // 2. find the {year}.lisp file in path folder, using ensure_directory_exists and ensure_file_exists
    crate::fs::ensure_directory_exists(&path)?;
    let filename = format!("{}.lisp", year);
    let file_path = path.join(&filename);
    crate::fs::ensure_file_exists(&file_path)?;

    // lock accounts here
    let mut a = accs.lock();
    let mut accs_ = a.as_mut().unwrap();

    tx.tx_id = Some(uuid::Uuid::new_v4().to_string());
    let lisp_str = tx.serialize_lisp()?;
    cal_transaction(&mut accs_, &tx)?; // update accounts
    // maybe I need some reverse operation. If the below writing failed, actually some data lost. The acc have updated, but not record

    // 3. store the new accounts/transaction
    let content_to_append = format!("{}\n", lisp_str);
    crate::fs::append_string_to_file(&content_to_append, path.clone(), &filename)?;
    let new_record = Record {
        accounts: accs_.iter().map(|a| a.clone()).collect(),
    };
    crate::fs::append_string_to_file(lisp_rpc_to_str(new_record)?.as_str(), path, "accounts.lisp")?;

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
        std::fs::create_dir_all(&test_dir).unwrap();

        let accounts_content = r#"(record :accounts
        '((account :name "Assets:Checking"
                   :balance 1000.0
                   :positive-op "expense")))"#;
        std::fs::write(test_dir.join("accounts.lisp"), accounts_content).unwrap();

        let tx_json = r#"{
            "timestamp": "2024-07-15T21:27:00-04:00[America/New_York]",
            "account": "Assets:Checking",
            "tx_type": "debit",
            "amount": 100.0,
            "category": ["Food", "Groceries"]
        }"#;
        let mut tx: Transaction = serde_json::from_str(tx_json).unwrap();

        entry(test_dir.clone(), &mut tx).unwrap();

        let lisp_file = test_dir.join("2024.lisp");
        assert!(lisp_file.is_file());

        let content = std::fs::read_to_string(lisp_file).unwrap();
        assert!(content.contains("transaction"));
        assert!(content.contains("Assets:Checking"));

        std::fs::remove_dir_all(&test_dir).unwrap();
    }

    #[test]
    fn test_parse_offset_only() {
        let ts1 = "2026-07-06T15:00:00-04:00";
        let parsed1 = ts1.parse::<jiff::civil::DateTime>();
        assert!(parsed1.is_ok(), "Failed to parse ts1: {:?}", parsed1.err());
        assert_eq!(parsed1.unwrap().year(), 2026);

        let ts2 = "2025-12-31T21:00:00-05:00[America/New_York]";
        let parsed2 = ts2.parse::<jiff::civil::DateTime>();
        assert!(parsed2.is_ok(), "Failed to parse ts2: {:?}", parsed2.err());
        assert_eq!(parsed2.unwrap().year(), 2025);
    }
}
