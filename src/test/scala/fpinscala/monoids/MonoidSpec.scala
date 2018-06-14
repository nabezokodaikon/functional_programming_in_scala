package fpinscala.monoids

import org.scalatest.FunSuite

class MonoidSpec extends FunSuite {
}

object Monoid {
}

trait Foldable[F[_]]

object ListFoldable[F[_]] {
}

object IndexedSeqFoldable[F[_]] {
}

object StreamFoldable[F[_]] {
}

object OptionFoldable[F[_]] {
}
