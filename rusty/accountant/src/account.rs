use accountant_rpc::Transaction;
use anyhow::Context;
use std::path::PathBuf;

use lisp_rpc_rust_raw_data::Data;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Record {
    pub accounts: Vec<Account>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Account {
    name: String,
    balance: f64,
    positive_op: String,

    /// last transaction id
    last_transaction: Option<String>,
}

pub fn account_file_path(dir: PathBuf) -> anyhow::Result<PathBuf> {
    crate::fs::ensure_directory_exists(&dir)?;
    let filename = "accounts.lisp";
    let file_path = dir.join(&filename);
    crate::fs::ensure_file_exists(&file_path)?;

    Ok(file_path)
}

/// Read the accounts file
pub fn load_accounts(path: PathBuf) -> anyhow::Result<Vec<Account>> {
    let file_path = account_file_path(path)?;

    let accounts_file = lisp_rpc_rust_raw_data::files::DataFile::new(file_path)?;

    let accounts = accounts_file
        .gen_table() // this will only keep the last expr
        .get("record")
        .context("cannot get the record data")?
        .get("accounts")
        .context("cannot get the accounts data")?;

    //dbg!(accounts);
    match accounts {
        // need some to_list function
        lisp_rpc_rust_raw_data::Data::List(ll) => Ok(ll
            .into_iter()
            .filter_map(|d| if let Data::Data(e) = d { Some(e) } else { None })
            .map(|d| Account {
                name: d.get("name").unwrap().get_string().unwrap(),
                balance: d.get("balance").unwrap().to_float().unwrap(),
                positive_op: d.get("positive-op").unwrap().get_string().unwrap(),

                last_transaction: d
                    .get("last-transaction")
                    .map_or(None, |d| Some(d.get_string().unwrap())),
            })
            .collect::<Vec<Account>>()),
        _ => anyhow::bail!("accounts has to be the list of account"),
    }
}

/// Link the accounts with the transaction
pub fn cal_transaction(accs: &mut [Account], tx: &Transaction) -> anyhow::Result<()> {
    if tx.tx_id.is_none() {
        anyhow::bail!("transaction id is nil")
    }

    if tx.tx_type == "transfer" {
        match tx.target_account.as_ref() {
            Some(target_a) => return handle_transfer(accs, target_a, tx),
            None => anyhow::bail!("transfer type has to have the target account"),
        }
    }

    // if it is not transfer, handle expense or income below
    let a = accs
        .iter_mut()
        .filter(|a| a.name == tx.account)
        .next()
        .ok_or(anyhow::anyhow!("cannot get the account {}", tx.account))?;

    a.last_transaction = tx.tx_id.clone();

    if a.positive_op == "expense" {
        a.balance += tx.amount
    } else {
        a.balance -= tx.amount
    }

    Ok(())
}

/// the function handle the transfer
pub fn handle_transfer(
    accs: &mut [Account],
    target_account: &str,
    tx: &Transaction,
) -> anyhow::Result<()> {
    let mut from_acc: Option<&mut Account> = None;
    let mut target_acc: Option<&mut Account> = None;

    for a in accs {
        if a.name == tx.account {
            from_acc = Some(a);
        } else if a.name == target_account {
            target_acc = Some(a);
        }
    }

    if from_acc.is_none() && target_acc.is_none() {
        anyhow::bail!("from/target accounts have to be found all of them");
    }

    let from_acc: &mut Account = from_acc.unwrap();
    let target_acc: &mut Account = target_acc.unwrap();

    match (
        from_acc.positive_op.as_str(),
        target_acc.positive_op.as_str(),
    ) {
        ("expense", "incoming") => {
            from_acc.balance += tx.amount;
            target_acc.balance += tx.amount;
        }
        ("incoming", "expense") => {
            from_acc.balance -= tx.amount;
            target_acc.balance -= tx.amount;
        }
        ("expense", "expense") => {
            from_acc.balance += tx.amount;
            target_acc.balance -= tx.amount;
        }
        ("incoming", "incoming") => {
            from_acc.balance -= tx.amount;
            target_acc.balance += tx.amount;
        }
        (a, b) => anyhow::bail!("some unsupport position_op {} and {}", a, b),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use lisp_rpc_rust_serializer::*;

    #[test] // this test actually just for printing
    fn test_load_accounts() {
        let a = load_accounts("./data".into()).unwrap();
        dbg!(&a);
        println!("{:?}", lisp_rpc_to_str(Record { accounts: a }));
    }
}
