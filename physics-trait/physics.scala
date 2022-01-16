package physics;

trait Physical[M <: Module[_,Double]] {
  type Position = (Double, Double)
  type Velocity = (Double, Double)
  type Acceleration = (Double, Double)

  def frame : M
  def position() : Position
  def velocity() : Velocity
  def acceleration() : Acceleration
}

// Something that can be stepped forward in time
trait Steppable {
  def step(dt : Double) : Unit
}

// Derivatives are used by ODE solvers
trait Derivative[M <: Module[_, Double]] {
  def d() : M
}

trait EulerForward[M <: Module[M,Double]] extends Steppable {
  this : Physical[M] with Derivative[M] =>
  def step(dt : Double) {
    frame += d() * dt
  }
}

class MyFrame (var x : Double, var y : Double, var vx : Double, var vy : Double) extends Module[MyFrame,Double] {
  def +=(other: MyFrame) {
    x += other.x
    y += other.y
    vx += other.vx
    vy += other.vy
  }
  def +(other: MyFrame) = {
    new MyFrame(x + other.x, y + other.y, vx + other.vx, vy + other.vy)
  }
  def *(s: Double) = {
    new MyFrame(x * s, y * s, vx * s, vy * s)
  }

  override def toString = x.toString + " " + y.toString
}
