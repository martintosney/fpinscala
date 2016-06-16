package fpinscala.errorhandling

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class OptionSpec extends FlatSpec {
  import Option._

  //4.1
  "map" should "return None when when used on a None" in {
    None.map(x => x) shouldBe None
  }

  it should "return a mapped Some when used on a Some" in {
    Some(1).map(x => x + 1) shouldBe Some(2)
  }

  "getOrElse" should "return the default for a None" in {
    None.getOrElse(1) shouldBe 1
  }

  it should "return the value in the Some for a Some" in {
    Some(2).getOrElse(1) shouldBe 2
  }

  "flatMap" should "return None when use on a None" in {
    None.flatMap(x => Some(1)) shouldBe None
  }

  it should "return a Some when used on a Some" in {
    Some(1).flatMap(x => Some(x+1)) shouldBe Some(2)
  }

  "orElse" should "return ob when given a None" in {
    None.orElse(Some(1)) shouldBe Some(1)
  }

  it should "return the original Some when used on a Some" in {
    Some(1).orElse(None) shouldBe Some(1)
  }

  "filter" should "return None for a None" in {
    None.filter(_ => true) shouldBe None
    None.filter(_ => false) shouldBe None
  }

  it should "return None for a Some when function maps to false" in {
    Some(1).filter(_ => false) shouldBe None
  }

  it should "return Some for a Some when function maps to true" in {
    Some(1).filter(_ => true) shouldBe Some(1)
  }

  //4.2
  "variance" should "return None when given an empty sequence" in {
    variance(Seq()) shouldBe None
  }

  it should "be Some(0) for a sequence with a single element" in {
    variance(Seq(2)) shouldBe Some(0)
  }

  it should "be 2/3 for Seq(1,2,3)" in {
    val v = variance(Seq(1,2,3)).getOrElse(0.0)
    assert(v > 0.666 && v < 0.667)
  }

  //4.3
  "map2" should "return None when first parameter is also a None" in {
    map2(None, Some(1))((a,b) => a.toString + b.toString) shouldBe None
  }

  it should "return None when second parameter is also a None" in {
    map2(Some(1), None)((a,b) => a.toString + b.toString) shouldBe None
  }

  it should "return a Some when both parameters are Somes" in {
    map2(Some(1), Some(2))((a,b) => a.toString + b.toString) shouldBe Some("12")
  }

  //4.4
  "sequence" should "turn an empty list into a Some(Nil)" in {
    sequence(List[Option[Int]]()) shouldBe Some(Nil)
  }

  it should "turn a list of three Somes into a Some" in {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
  }

  it should "turn a list containing at least one None into a None" in {
    sequence(List(None, Some(1), Some(2))) shouldBe None
  }

  //4.5
  "traverse" should "return Some(Nil) when used on empty list" in {
    traverse(List())(a => Some(a)) shouldBe Some(Nil)
  }

  def evenOpt(i: Int) : Option[Int] = if (i%2==0) Some(i) else None

  it should "return a list of even numbers" in {
    traverse(List(2,4,6,8))(evenOpt) shouldBe Some(List(2,4,6,8))
  }

  it should "return None for a list with an odd number" in {
    traverse(List(2,4,6,7))(evenOpt) shouldBe None
  }
}
