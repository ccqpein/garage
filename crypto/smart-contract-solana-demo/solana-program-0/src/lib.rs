use solana_program::{
    account_info::{AccountInfo, next_account_info},
    entrypoint,
    entrypoint::ProgramResult,
    msg,
    pubkey::Pubkey,
};

entrypoint!(process_instruction);

/// Processes a Solanana instruction
pub fn process_instruction(
    // Public key of the account this program was loaded into
    program_id: &Pubkey,
    // Accounts required for the instruction to run
    accounts: &[AccountInfo],
    instruction_data: &[u8],
) -> ProgramResult {
    msg!("Hello, Solana!");
    //msg!("which public key? {:?}", program_id);

    let accounts_iter = &mut accounts.iter();

    let account = next_account_info(accounts_iter)?;

    msg!("Received instruction data: {:?}", instruction_data);
    msg!("Hello from {:?}", account.key);

    Ok(())
}
