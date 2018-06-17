package fpinscala.iomonad

import org.scalatest.FunSuite

class IOSpec extends FunSuite {

  test("List 13-1") {
    Player.contest(Player("taro", 100), Player("jiro", 90))
    Player.contest(Player("taro", 50), Player("jiro", 100))
    Player.contest(Player("taro", 50), Player("jiro", 50))

    Player.contest2(Player("taro", 100), Player("jiro", 90))
    Player.contest2(Player("taro", 50), Player("jiro", 100))
    Player.contest2(Player("taro", 50), Player("jiro", 50))

    Player.contest3(Player("taro", 100), Player("jiro", 90))
    Player.contest3(Player("taro", 50), Player("jiro", 100))
    Player.contest3(Player("taro", 50), Player("jiro", 50))
  }
}
