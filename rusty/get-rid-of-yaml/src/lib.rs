use std::io::BufRead;

/// read until the colon and split the buffer on it
fn split_on_colon<R>(reader: R, buf: &mut Vec<u8>)
where
    R: BufRead,
{
	let length = reader.
    match reader.read_until(b':', buf) {
        Ok(n) if n == => todo!(),
        Err(_) => todo!(),
    }
}
