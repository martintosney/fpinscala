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

  "drop" should "return the Stream when argument is zero" in {
    (Stream(1,2,3) drop 0).toList shouldBe List(1,2,3)
  }

  it should "return a partial Stream" in {
    (Stream(1,2,3) drop 2).toList shouldBe List(3)
  }

  it should "return Empty when argument is greater than or equal to Stream length" in {
    Stream(1,2,3) drop 4 shouldBe Empty
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

  //5.8
  "constant" should "give us a bunch of 2s" in {
    (constant(2) take 5).toList shouldBe List(2,2,2,2,2)
  }

  //5.9
  "from" should "generate a stream starting from 4" in {
    (from(5) take 5).toList shouldBe List(5,6,7,8,9)
  }

  //5.10
  "fibs" should "return fibonacci numbers" in {
    (fibs take 7).toList shouldBe List(0,1,1,2,3,5,8)
  }

  //5.11
  "unfold" should "generate an empty stream if the function always returns None" in {
     unfold(1)(a => None) shouldBe Empty
  }

  it should "generate an infinite Stream of alternating 1s and -1s" in {
    ( unfold(-1)(a => Some((-a, -a))) take 5).toList shouldBe List(1,-1,1,-1,1)
  }

  it should "generate a finite stream, counting from 5 to 1" in {
    ( unfold(5)(a => if (a > 0) Some((a,a-1)) else None) take 10).toList shouldBe List(5,4,3,2,1)
  }

  //5.12
  "onesUnfold" should "behave the same as ones" in {
    (onesUnfold take 50).toList shouldBe (ones take 50).toList
  }

  "constantUnfold" should "behave the same as constant" in {
    (constantUnfold(10) take 50).toList shouldBe (constant(10) take 50).toList
  }

  "fromUnfold" should "behave the same as from" in {
    (from(10) take 50).toList shouldBe (from(10) take 50).toList
  }

  "fibsUnfold" should "behave the same as fibs" in {
    (fibsUnfold take 20).toList shouldBe (fibs take 20).toList
  }

  //5.13
  "mapUnfold" should "behave the same as map" in {
    ((from(1) mapUnfold (x => x+1)) take 50).toList shouldBe 
      ((from(1) map (x => x+1)) take 50).toList 
  }

  "takeUnfold" should "behave the same as take" in {
    (from(1) takeUnfold 50).toList shouldBe
      (from(1) take 50).toList
  }

  "takeWhileUnfold" should "behave the same as takeWhile" in {
    (from(1) takeWhileUnfold (_<50)).toList shouldBe
      (from(1) takeWhile (_<50)).toList
  }

  def zipAdd(a: Int, b: Int): Int = a + b
  "zipWith" should "behave zip together two Streams" in {
    (from(1).zipWith(from(1))(zipAdd) take 5).toList shouldBe 
      List(2,4,6,8,10)
  }

  "zipAll" should "combine two streams of different sizes" in {
    def intStream = from(1) take 5
    def charStream = Stream('a', 'b', 'c')
    (intStream zipAll charStream).toList shouldBe 
      List((Some(1),Some('a')), (Some(2),Some('b')), (Some(3),Some('c')),
        (Some(4),None), (Some(5), None))
  }
}
