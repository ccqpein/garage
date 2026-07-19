use accountant::entry;
use accountant_rpc::Transaction;
use lisp_rpc_rust_serializer;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_integration_entry() -> anyhow::Result<()> {
    let temp_dir = PathBuf::from("tests").join("temp");

    // Clean up from any previous failed runs
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir).ok();
    }
    fs::create_dir_all(&temp_dir).unwrap();

    let accounts_content = r#"(record :accounts
    '((account :name "vv"
               :balance 1000.0
               :positive-op "expense")))"#;
    fs::write(temp_dir.join("accounts.lisp"), accounts_content).unwrap();

    let tx = r#"(transaction
                  :timestamp "2026-07-15T21:27:00+00:00[UTC]"
                  :account "vv"
                  :tx-type "cc"
                  :amount 1
                  :category '("a" "b"))"#;
    let mut tx: Transaction = lisp_rpc_rust_serializer::lisp_rpc_from_str(tx).unwrap();

    // Call the entry function
    entry(temp_dir.clone(), &mut tx).unwrap();

    // Verify file creation and content
    let lisp_file = temp_dir.join("2026.lisp"); // the file that the same year as the timestamp
    assert!(lisp_file.is_file());

    let content = fs::read_to_string(&lisp_file).unwrap();

    let tx1 = lisp_rpc_rust_serializer::lisp_rpc_from_str(&content)?; // only one line of files tho
    assert_eq!(tx, tx1);

    // Clean up
    fs::remove_dir_all(&temp_dir)?;
    Ok(())
}

#[test]
fn test_integration_entry_offset_only() -> anyhow::Result<()> {
    let temp_dir = PathBuf::from("tests").join("temp_offset");

    // Clean up from any previous failed runs
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir).ok();
    }
    fs::create_dir_all(&temp_dir).unwrap();

    let accounts_content = r#"(record :accounts
    '((account :name "vv"
               :balance 1000.0
               :positive-op "expense")))"#;
    fs::write(temp_dir.join("accounts.lisp"), accounts_content).unwrap();

    let tx = r#"(transaction
                  :timestamp "2026-07-06T15:00:00-04:00"
                  :account "vv"
                  :tx-type "cc"
                  :amount 1
                  :category '("a" "b"))"#;
    let mut tx: Transaction = lisp_rpc_rust_serializer::lisp_rpc_from_str(tx).unwrap();

    // Call the entry function
    entry(temp_dir.clone(), &mut tx).unwrap();

    // Verify file creation and content
    let lisp_file = temp_dir.join("2026.lisp");
    assert!(lisp_file.is_file());

    let content = fs::read_to_string(&lisp_file).unwrap();

    let tx1 = lisp_rpc_rust_serializer::lisp_rpc_from_str(&content)?;
    assert_eq!(tx, tx1);

    // Clean up
    fs::remove_dir_all(&temp_dir)?;
    Ok(())
}

