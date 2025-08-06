use solana_client::rpc_client::RpcClient;
use solana_sdk::{
    instruction::{AccountMeta, Instruction},
    message::Message,
    pubkey::Pubkey,
    signature::{Keypair, Signer},
    system_program,
    transaction::Transaction,
};
use std::{fs, str::FromStr, time::Duration};
use tokio::time::sleep;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let rpc_url = "http://localhost:8899"; // local Solana validator URL
    let client = RpcClient::new(rpc_url.to_string());

    let program_id = Pubkey::from_str(&include_str!("../programID"))?;
    println!("program_id is: {program_id}");

    let payer = Keypair::new();
    println!("Client Payer Public Key: {}", payer.pubkey());

    println!("Airdropping SOL to client payer...");
    let airdrop_signature = client.request_airdrop(&payer.pubkey(), 1_000_000_000)?; // 1 SOL

    // Explicitly wait for the airdrop transaction to be confirmed
    //client.confirm_transaction(&airdrop_signature)?;
    // making sure the airdrop done
    loop {
        if client.confirm_transaction(&airdrop_signature)? {
            break;
        }
        sleep(Duration::from_secs(1)).await;
    }
    println!("Airdrop request sent and confirmed by network.");

    // --- ENHANCED: Wait for balance to update and be sufficient ---
    // let target_balance_lamports = 1_000_000_000; // 1 SOL
    // let mut current_balance_lamports = 0;
    // let mut attempts = 0;
    // let max_attempts = 30; // Wait up to 30 seconds (30 * 1 sec)

    // while current_balance_lamports < target_balance_lamports && attempts < max_attempts {
    //     current_balance_lamports = client.get_balance(&payer.pubkey())?;
    //     if current_balance_lamports >= target_balance_lamports {
    //         println!(
    //             "Balance sufficient: {} SOL",
    //             current_balance_lamports as f64 / 1_000_000_000.0
    //         );
    //         break;
    //     }
    //     attempts += 1;
    //     eprintln!(
    //         "Attempt {}/{}: Waiting for sufficient balance (Current: {} SOL). Retrying in 1 second...",
    //         attempts,
    //         max_attempts,
    //         current_balance_lamports as f64 / 1_000_000_000.0
    //     );
    //     sleep(Duration::from_secs(1)).await;
    // }

    // if current_balance_lamports < target_balance_lamports {
    //     return Err(format!(
    //         "Failed to get sufficient SOL balance after airdrop. Current: {} SOL. Airdrop might have failed or validator is very slow.",
    //         current_balance_lamports as f64 / 1_000_000_000.0
    //     ).into());
    // }
    // --- END ENHANCED SECTION ---

    // --- Prepare and Send Transaction (rest of your code) ---
    let instruction_data = vec![];
    let instruction = Instruction {
        program_id,
        accounts: vec![AccountMeta::new(payer.pubkey(), true)],
        data: instruction_data,
    };

    let recent_blockhash = client.get_latest_blockhash()?;
    println!("Recent Blockhash: {}", recent_blockhash);

    // make message that with instruction and client the public key
    let message = Message::new(&[instruction], Some(&payer.pubkey()));

    // payer: key pair
    let transaction = Transaction::new(&[&payer], message, recent_blockhash);

    println!("Sending transaction...");
    let signature = client.send_and_confirm_transaction(&transaction)?;
    println!("Transaction sent! Signature: {}", signature);

    println!(
        "\nCheck the terminal running `solana-test-validator` for the 'Hello, Solana!' log messages."
    );
    println!(
        "You can also view transaction details with: solana transaction {} --cluster localhost",
        signature
    );

    Ok(())
}
