use std::ops::Fn;
use std::pin::Pin;
use std::future::Future;
use std::task::{Context, Poll};
use std::ops::*;

// Example structs...
struct User(String, String, u32);

struct User2 {
    name: String,
    value: u32
}

// newtype on an int so we can use it for type stuff
#[derive(Debug, Copy, Clone)]
struct Example(i32);

// Frenet Frame, or TNB frame
struct FrenetFrame {
    tangent: f32,
    normal: f32,
    binormal: f32
}

// Position Velocity Acceleration frame
struct PVA {
    x: f32,
    y: f32
}

fn len<T>(list: &[T]) -> usize {
    list.len()
}

// The simplest trait, has a function "test" which returns an int
trait MyTraitA {
    fn test(&self) -> u32;
}
impl MyTraitA for User2 {
    fn test(&self) -> u32 {
        self.value
    }
}


// A Magma is just a set with a binary operator
trait AddMagma: Sized + Clone + Add<Self, Output=Self> + AddAssign<Self> {}

impl Add<Self> for Example {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Example(3)
    }
}
impl AddAssign<Self> for Example {
    fn add_assign(&mut self, rhs: Self) {
        *self = Self(self.0 + rhs.0)
    }
}
impl AddMagma for Example {}


// Marker for addition to be associative
trait AddAssociative {}

// Marker for addition to be commutative
trait AddCommutative {}

// A semigroup is an associative magma
trait AddSemigroup: AddMagma + AddAssociative {}

// Additive identity element 
trait Zero {
    fn zero() -> Self;
}

// A monoid is a semigroup with an identity element
trait AddMonoid: AddMagma + Zero {}


// Inverse elements
trait Negatable: Sized + Clone + Neg<Output=Self> + Sub<Self, Output=Self> + SubAssign<Self> {}

// A group is a monoid with inverse elements (a set with a binary operator which is associative,
// has an identity element, and inverse elements)
trait AddGroup: AddMonoid + Negatable {}

// Finally, an albelian group is a commutative additive group
trait AddAbelianGroup: AddGroup + AddCommutative {}

impl std::ops::Add<Self> for PVA {
    type Output = PVA;
    fn add(self, rhs: PVA) -> PVA {
        PVA {
            x: 1.0,
            y: 1.0
        }
    }
}

impl std::ops::Mul<f32> for PVA {
    type Output = PVA;
    fn mul(self, rhs: f32) -> PVA {
        PVA {
            x: 1.0,
            y: 1.0
        }
    }
}

//impl Module<f32> for PVA {
//}

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
}
