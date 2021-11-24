#![no_std]
#![no_main]

extern crate libc;


fn epoll_create() -> Result<i32,i32> {
    let res: i32 = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
    if -1 == res {
        Err(-1)
    } else {
        Ok(res)
    }
}


fn foo() -> Result<i32,i32> {
    epoll_create()?;
    Ok(0)
}


#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    const HELLO: &'static str = "Hello, World\n\0";
    unsafe {
        libc::printf(HELLO.as_ptr() as *const _);
    }
    match foo() {
        Ok(_) => 0,
        Err(_) => 1
    };
    0
}

#[panic_handler]
fn my_panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
