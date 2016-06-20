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
  "take" should "return Empty, no matter how many elements are taken" in {
    Empty take 0 shouldBe Empty
    Empty take 1 shouldBe Empty
  }

  it should "return an empty Stream when passed 0" in {
    Stream(1,2,3) take 0 shouldBe Empty
  }

  it should "return the first X elements when taking X" in {
    println((Stream(1,2,3) take 2).toList)
    (Stream(1,2,3) take 2).toList shouldBe Stream(1,2).toList
  }

}
