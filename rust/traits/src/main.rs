use std::ops::Fn;
use std::pin::Pin;
use std::future::Future;
use std::task::{Context, Poll};
use futures::executor::block_on;

struct User(String, String, u32);

fn len<T>(list: &[T]) -> usize {
    list.len()
}

trait Functora<A> {
    fn map<F>(self: Self, f: F) -> A where
        F: Fn(A) -> A;
}

//trait Functorb<A> {
//    fn map<B>(self: Self, f: fn(A) -> B) -> Functorb<B>;
//}

//struct MyBox<A> {
//    v: A
//}
//impl<T> Functorb<T> for MyBox<T> {
//    fn map<U>(self: Self, f: fn(T) -> U) -> MyBox<U> {
//        MyBox { v: f(self.v) }
//    }
//}

async fn foo() -> u8 { 5 }

struct TimerFuture;

impl Future for TimerFuture {
    type Output = u8;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Pending
    }
}

fn timer() -> TimerFuture {
    TimerFuture
}

async fn bar() -> u8 {
    timer().await
}

fn main() {
    println!("Hello, world!");
    let list = vec![34, 50, 12];
    let first = len(&list);
    println!("first={}", first);

    let f = async {
        let x: u8 = foo().await;
        println!("x={}", x+5);
    };

    blo
}
