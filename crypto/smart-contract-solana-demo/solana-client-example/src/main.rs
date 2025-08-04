use solana_client::rpc_client::RpcClient;
use solana_sdk::{
    instruction::{AccountMeta, Instruction},
    message::Message,
    pubkey::Pubkey,
    signature::{Keypair, Signer},
    system_program,
    transaction::Transaction,
};
use std::{fs, str::FromStr};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let rpc_url = "http://localhost:8899"; // local Solana validator URL
    let client = RpcClient::new(rpc_url.to_string());

    let program_id = Pubkey::from_str(&include_str!("../programID"))?;
    println!("program_id is: {program_id}");

    // Generate a new keypair for the client to send the transaction
    let payer = Keypair::new();
    println!("Client Payer Public Key: {}", payer.pubkey());

    // --- 2. Fund the Payer Account (for local development) ---
    // On devnet/testnet, you'd use a faucet. On localnet, you can airdrop.
    println!("Airdropping SOL to client payer...");

    let signature = client.request_airdrop(&payer.pubkey(), 1_000_000_000)?; // 1 SOL (in lamports)
    client.confirm_transaction(&signature)?; // Wait for the airdrop to confirm
    println!(
        "Airdrop confirmed. Balance: {} SOL",
        client.get_balance(&payer.pubkey())? / 1_000_000_000
    );

    // --- 3. Prepare the Instruction ---
    // Our 'my-solana-program' simply logs "Hello, Solana!" and the account key.
    // It requires one account: the account to say hello to (which we can make the payer itself).
    // The instruction_data is empty for this simple program, as it doesn't parse any.

    let instruction_data = vec![]; // Empty instruction data for our simple program

    let instruction = Instruction {
        program_id, // The ID of our deployed program
        accounts: vec![
            AccountMeta::new(payer.pubkey(), true), // The payer account, marked as writable and signer
                                                    // Add any other accounts your program expects.
                                                    // Our "Hello, Solana!" program only uses one account from `accounts: &[AccountInfo]`.
                                                    // We pass the payer's account here so the program can log its public key.
        ],
        data: instruction_data,
    };

    // --- 4. Create and Send the Transaction ---
    let recent_blockhash = client.get_latest_blockhash()?;
    println!("Recent Blockhash: {}", recent_blockhash);

    let message = Message::new(&[instruction], Some(&payer.pubkey()));
    let transaction = Transaction::new(&[&payer], message, recent_blockhash);

    println!("Sending transaction...");
    let signature = client.send_and_confirm_transaction(&transaction)?;
    println!("Transaction sent! Signature: {}", signature);

    // --- 5. Verify Logs (Optional, but good for debugging) ---
    // You can fetch transaction details to see the logs programmatically.
    // However, for this simple case, the `solana-test-validator` terminal is easier.
    println!(
        "\nCheck the terminal running `solana-test-validator` for the 'Hello, Solana!' log messages."
    );
    println!(
        "You can also view transaction details with: solana transaction {} --cluster localhost",
        signature
    );

    Ok(())
}
