package fpinscala.laziness

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StreamSpec extends FlatSpec with Matchers {
  import Stream._

  //5.1
  "toList" should "return Nil when used on an empty stream" in {
    Empty.toList shouldBe Nil
  }

  it should "return an equivalent list when used on a stream with a few elements" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
  }

  //5.2
  // "take" should "return an empty Stream when passed 0" in {
  //   assert
}
