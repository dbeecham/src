package data.structures;

trait Module[M <: Module[_,F],F] {
  this : M =>
  def +=(m: M) : Unit
  def +(m: M) : M
  def *(f: F) : M
}
