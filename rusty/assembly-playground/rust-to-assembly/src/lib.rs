#![cfg_attr(target_os = "none", no_std)] // for taget `thumbv8m.main-none-eabihf`

/// for assembly name unchange
#[unsafe(no_mangle)]
#[cfg(not(target_os = "none"))]
pub fn peak_index_in_mountain_array(arr: Vec<i32>) -> i32 {
    helper(&arr) as i32
}

#[cfg(not(target_os = "none"))]
fn helper(arr: &[i32]) -> usize {
    if arr.len() == 1 {
        return 0;
    }

    if arr[0] > arr[1] {
        return 0;
    }

    if arr.len() == 2 {
        return 1;
    }

    let mid = arr.len() / 2;
    let ai = helper(&arr[..mid]);
    let bi = helper(&arr[mid..]) + mid;

    if arr[ai] > arr[bi] {
        return ai;
    } else {
        return bi;
    }
}

#[unsafe(no_mangle)]
#[cfg(target_os = "none")]
pub fn add_arrays_mve(a: &[i32], b: &[i32], out: &mut [i32]) {
    for ((&x, &y), z) in a.iter().zip(b.iter()).zip(out.iter_mut()) {
        *z = x + y;
    }
}

// this one for VPT Predication
#[unsafe(no_mangle)]
#[cfg(target_os = "none")]
pub fn relu_mve(data: &mut [f32]) {
    for x in data.iter_mut() {
        if *x < 0.0 {
            *x = 0.0;
        }
    }
}

// #[cfg(not(target_os = "none"))]
// fn main() {
//     assert_eq!(peak_index_in_mountain_array(vec![0, 1, 0]), 1);
//     assert_eq!(peak_index_in_mountain_array(vec![0, 2, 1, 0]), 1);
//     assert_eq!(peak_index_in_mountain_array(vec![0, 10, 5, 2]), 1);
// }

#[cfg(target_os = "none")]
use core::panic::PanicInfo;

#[cfg(target_os = "none")]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// #[cfg(target_os = "none")]
// fn main() {}
