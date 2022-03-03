#![no_std]
#![no_main]

extern crate libc;
extern crate alloc;

use core::task::{ Context, Poll, Waker, RawWaker, RawWakerVTable };
use core::future::Future;
use core::pin::Pin;
use alloc::boxed::Box;
use alloc::collections::VecDeque;
use pin_utils::pin_mut;

use std::alloc::System;

#[global_allocator]
static GLOBAL: System = System;

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


async fn timer() -> u8 {
    8
}

fn bar<'a>() -> impl Future<Output = ()> + 'a {
    unsafe { libc::printf("outside async\n\0".as_ptr() as *const _); }
    async {
        unsafe { libc::printf("inside async\n\0".as_ptr() as *const _); }
    }
}



fn my_raw_waker() -> RawWaker {
    fn no_op(_: *const ()) {}
    fn clone(_: *const ()) -> RawWaker {
        unsafe { libc::printf("in rawwaker clonse\n\0".as_ptr() as *const _); }
        my_raw_waker()
    }

    let vtable = &RawWakerVTable::new(clone, no_op, no_op, no_op);
    RawWaker::new(0 as *const (), vtable)
}

fn my_waker() -> Waker {
    unsafe { Waker::from_raw(my_raw_waker()) }
}


//pub struct Task {
//    future: Pin<Box<dyn Future<Output = ()>>>
//}
//
//impl Task {
//    pub fn new(future: impl Future<Output = ()> + 'static) -> Task {
//        Task {
//            future: Box::pin(future)
//        }
//    }
//
//    fn poll(&mut self, context: &mut Context) -> Poll<()> {
//        self.future.as_mut().poll(context)
//    }
//}


//pub struct Executor {
//    task_queue: VecDeque<Task>
//}
//
//impl Executor {
//    pub fn new() -> Executor {
//        Executor {
//            task_queue: VecDeque::new()
//        }
//    }
//
//    pub fn spawn(&mut self, task: Task) {
//        self.task_queue.push_back(task)
//    }
//
//    pub fn run(&mut self) {
//        let waker = my_waker();
//        let mut context = Context::from_waker(&waker);
//        match self.task_queue.pop_front() {
//            Some(mut task) => {
//                task.poll(&mut context);
//            }
//            Nothing => {
//                unsafe { libc::printf("executor::run gave Nothing\n\0".as_ptr() as *const _); }
//            }
//        }
//    }
//}


#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {

    const HELLO: &'static str = "Hello, World\n\0";
    const NO: &'static str = "Nope\n\0";

    unsafe {
        libc::printf("Hello, world\n\0".as_ptr() as *const _);
    }

//    let mut executor = Executor::new();
//    executor.spawn(Task::new(bar()));
//    executor.run();

    let waker = my_waker();
    let mut context = Context::from_waker(&waker);
//    let f = Box::pin(async { 
//        unsafe { libc::printf("what!\n\0".as_ptr() as *const _); }
//    }).as_mut().poll(&mut context);
//
//    let a = async { 8 };

    Future::poll(Box::pin(async { 8 }).as_mut(), &mut context);

    Future::poll(unsafe { Pin::new_unchecked(&mut async { 8 }) }, &mut context);

    unsafe { Pin::new_unchecked(&mut async { 8 }) }.poll(&mut context);

    //let b = core::pin::Pin::new(a);
    
    //let inner = core::pin::Pin::new(&mut bar);
    //let inner = &mut async { 8 };

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

//#[panic_handler]
//fn my_panic(_info: &core::panic::PanicInfo) -> ! {
//    loop {}
//}
