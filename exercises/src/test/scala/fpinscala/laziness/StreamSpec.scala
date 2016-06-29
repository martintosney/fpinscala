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
    (Stream(1,2,3) take 2).toList shouldBe List(1,2)
  }

  //5.3
  "takeWhile" should "return Empty if the predicate always evaluates to false" in {
    Stream(1,2,3) takeWhile (a => false) shouldBe Empty
  }

  it should "return Elements while the predicate evaluates to true" in {
    (Stream(1,2,3) takeWhile (a => a<3)).toList shouldBe List(1,2)
  }

  //5.4
  "forAll" should "return false when the predicate is always false" in {
    Stream(1,2,3) forAll (a => false) shouldBe false
  }

  it should "return true when the predicate is always true" in {
    Stream(1,2,3) forAll (a => true) shouldBe true
  }

  it should "return true if all of the elements match the predicate" in {
    Stream(1,2,3) forAll (a => a<5) shouldBe true
  }

  it should "return false if at least one element fails the predicate" in {
    Stream(1,2,3) forAll (a => a<3) shouldBe false
  }

  //5.5 
  "takeWhileFolded" should "behave the same as takeWhile" in {
    (Stream(1,2,3) takeWhileFolded (a => false)) shouldBe (Stream(1,2,3) takeWhile (a => false))
    (Stream(1,2,3) takeWhileFolded (a => a<3)).toList shouldBe (Stream(1,2,3) takeWhile (a => a<3)).toList
  }

  //5.6
  "headOption" should "return None when the Stream is Empty" in {
    Empty.headOption shouldBe None
  }

  it should "return a Some containing the first element of the Stream" in {
    Stream(1,2,3).headOption shouldBe Some(1)
  }

  //5.7
  "map" should "tranform each element of the stream" in {
    (Stream(1,2,3) map (a => a + 1)).toList shouldBe List(2,3,4)
  }

  "filter" should "return a stream containing only the elements matching the function" in {
    (Stream(1,2,3) filter (a => a%2==0)).toList shouldBe List(2)
  }

  "append" should "add an item to the end of a Stream" in {
    (Stream(1,2,3) append Stream(4)).toList shouldBe List(1,2,3,4)
  }

  "flatMap" should "do the mapping" in {
    (Stream(1,2) flatMap (a => cons(a.toString, cons(a.toString, Empty)))).toList shouldBe
      List("1","1","2","2")
  }
}
