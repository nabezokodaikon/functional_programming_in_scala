package fpinscala.monoids

trait Monoid[A] {

  // List 10-1
  def op(a1: A, a2: A): A
  def zero: A
}
