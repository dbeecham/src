#![no_std]
#![no_main]

extern crate libc;
//extern crate alloc;

//#[global_allocator]
//static GLOBAL: System = System;

struct Epoll {
    fd: i32
}

impl Epoll {
    fn new() -> Result<Epoll,i32> {
        let res: i32 = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
        match res {
            -1 => Err(0),
            _ => Ok(Epoll { fd: res })
        }
    }
}

fn foo() -> Result<i32,i32> {
    let epoll = Epoll::new()?;
    Ok(0)
}


#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {

    const HELLO: &'static str = "Hello, World\n\0";
    const NO: &'static str = "Nope\n\0";

    unsafe {
        libc::printf("Hello, world\n\0".as_ptr() as *const _);
    }

    match foo() {
        Ok(_) => {
            unsafe {
                libc::printf(HELLO.as_ptr() as *const _);
            }
        },
        Err(i) => {
            unsafe {
                libc::printf(NO.as_ptr() as *const _);
                libc::printf("Error was: %s\n\0".as_ptr() as *const _, libc::strerror(i));
            }
        }
    };
    0
}

#[panic_handler]
fn my_panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
