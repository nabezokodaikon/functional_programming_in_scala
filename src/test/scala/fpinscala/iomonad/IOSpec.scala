package fpinscala.iomonad

import org.scalatest.FunSuite

class IOSpec extends FunSuite {

  test("List 13-1") {
    val p1 = Player("taro", 10)
    val p2 = Player("jiro", 5)
    Player.contest(p1, p2)
    Player.contest2(p1, p2)
    Player.contest3(p1, p2)
    Player.contest4(p1, p2).run
  }

}
