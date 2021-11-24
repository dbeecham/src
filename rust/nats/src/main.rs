use nats;

fn main() {
    println!("Hello, world!");

    let nc : u8 = nats::connect("localhost:4222")?;
}
