use solana_program::{
    account_info::{next_account_info, AccountInfo},
    entrypoint,
    entrypoint::ProgramResult,
    msg,
    pubkey::Pubkey,
};

entrypoint!(process_instruction);

/// Processes a Solanana instruction
pub fn process_instruction(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    instruction_data: &[u8],
) -> ProgramResult {
    msg!("Hello, Solana!");

    let accounts_iter = &mut accounts.iter();

    let account = next_account_info(accounts_iter)?;

    if account.owner != program_id {
        msg!("Greeted account does not have the correct program id");
        return Err(solana_program::program_error::ProgramError::IncorrectProgramId);
    }

    msg!("Received instruction data: {:?}", instruction_data);
    msg!("Hello from {:?}", account.key);

    Ok(())
}
