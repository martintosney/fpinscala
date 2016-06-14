package fpinscala.errorhandling

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class OptionSpec extends FlatSpec {
  import Option._

  //4.1
  "map" should "return None when when used on a None" in {
    assert(None.map(x => x) === None)
  }

  it should "return a mapped Some when used on a Some" in {
    assert(Some(1).map(x => x + 1) === Some(2))
  }

  "getOrElse" should "return the default for a None" in {
    assert(None.getOrElse(1) === 1)
  }

  it should "return the value in the Some for a Some" in {
    assert(Some(2).getOrElse(1) === 2)
  }

  "flatMap" should "return None when use on a None" in {
    assert(None.flatMap(x => Some(1)) === None)
  }

  it should "return a Some when used on a Some" in {
    assert(Some(1).flatMap(x => Some(x+1)) === Some(2))
  }

  "orElse" should "return ob when given a None" in {
    assert(None.orElse(Some(1)) === Some(1))
  }

  it should "return the original Some when used on a Some" in {
    assert(Some(1).orElse(None) === Some(1))
  }

  "filter" should "return None for a None" in {
    assert(None.filter(_ => true) === None)
    assert(None.filter(_ => false) === None)
  }

  it should "return None for a Some when function maps to false" in {
    assert(Some(1).filter(_ => false) === None)
  }

  it should "return Some for a Some when function maps to true" in {
    assert(Some(1).filter(_ => true) === Some(1))
  }

  //4.2
  "variance" should "return None when given an empty sequence" in {
    assert(variance(Seq()) === None)
  }

  it should "be Some(0) for a sequence with a single element" in {
    assert(variance(Seq(2)) === Some(0))
  }

  it should "be 2/3 for Seq(1,2,3)" in {
    val v = variance(Seq(1,2,3)).getOrElse(0.0)
    assert(v > 0.666 && v < 0.667)
  }

  //4.3
  "map2" should "return None when first parameter is also a None" in {
    assert(map2(None, Some(1))((a,b) => a.toString + b.toString) === None)
  }

  it should "return None when second parameter is also a None" in {
    assert(map2(Some(1), None)((a,b) => a.toString + b.toString) === None)
  }

  it should "return a Some when both parameters are Somes" in {
    assert(map2(Some(1), Some(2))((a,b) => a.toString + b.toString) === Some("12"))
  }

  //4.4
  "sequence" should "turn an empty list into a Some(Nil)" in {
    assert(sequence(List[Option[Int]]()) === Some(Nil))
  }

  it should "turn a list of three Somes into a Some" in {
    assert(sequence(List(Some(1), Some(2), Some(3))) === Some(List(1,2,3)))
  }

  it should "turn a list containing at least one None into a None" in {
    assert(sequence(List(None, Some(1), Some(2))) === None)
  }

  //4.5
  "traverse" should "return Some(Nil) when used on empty list" in {
    assert(traverse(List())(a => Some(a)) === Some(Nil))
  }

  def evenOpt(i: Int) : Option[Int] = if (i%2==0) Some(i) else None

  it should "return a list of even numbers" in {
    assert(traverse(List(2,4,6,8))(evenOpt) === Some(List(2,4,6,8)))
  }

  it should "return None for a list with an odd number" in {
    assert(traverse(List(2,4,6,7))(evenOpt) === None)
  }
}
