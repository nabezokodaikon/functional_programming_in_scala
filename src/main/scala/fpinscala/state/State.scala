package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegariveInt(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegariveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nonNegariveInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
}

object State {
}
