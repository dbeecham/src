use nats;

fn main() -> void {
    let nc = nats::connect("localhost")?;
    nc.publish("topic", "message");
}
