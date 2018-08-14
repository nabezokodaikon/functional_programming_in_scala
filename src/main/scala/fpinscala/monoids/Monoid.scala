package fpinscala.monoids

// List 10-1
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
