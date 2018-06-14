package fpinscala.monoids

// List 10-1
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
}

// trait Foldable[F[_]]

// object ListFoldable[F[_]] {
// }

// object IndexedSeqFoldable[F[_]] {
// }

// object StreamFoldable[F[_]] {
// }

// object OptionFoldable[F[_]] {
// }
