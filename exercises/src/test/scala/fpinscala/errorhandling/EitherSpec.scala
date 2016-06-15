package fpinscala.errorhandling

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class EitherSpec extends FlatSpec {
  import Either._

  //4.6
  def increment(i: Int) : Int = i + 1

  "map" should "return the Left value when used on a Left" in {
    assert((Left("Error") map increment) === Left("Error"))
  }

  it should "apply the function when used on a Right" in {
    assert((Right(5) map increment) === Right(6))
  }

  def incrementIfEven(i: Int) : Either[String, Int] = 
    if (i%2==0) Right(i+1) else Left("Increment Error " + i.toString)

  "flatmap" should "return the Left value when used on a Left" in {
    assert((Left("Error") flatMap incrementIfEven) === Left("Error"))
  }

  it should "return a Left when used on an odd value" in {
    assert((Right(5) flatMap incrementIfEven) === Left("Increment Error 5"))
  }

  it should "return a Right when used on an even value" in {
    assert((Right(6) flatMap incrementIfEven) === Right(7))
  }

  "orElse" should "return the Right when used on a Right" in {
    assert((Right(7) orElse Right(4)) === Right(7))
  }

  it should "return the Right when used on a Left" in {
    assert((Left("An error happened") orElse Right(3)) === Right(3))
  }

  def add (a: Int, b: Int) : Int = a + b

  "map2" should "return the first Left" in {
    assert((Left("L1").map2(Left("L2"))(add)) === Left("L1"))
  }

  it should "return the second Left" in {
    assert((Right(1).map2(Left("L2"))(add)) === Left("L2"))
  }

  it should "return the applied function given two Rights" in {
    assert((Right(1).map2(Right(2))(add)) === Right(3))
  }

  //4.7
  "traverse" should "return the first Left encountered when applied" in {
    assert(traverse(List(2,3,4,5))(incrementIfEven) === Left("Increment Error 3"))
  }

  it should "return the mapped list if no errors" in {
    assert(traverse(List(2,4,6,8))(incrementIfEven) === Right(List(3,5,7,9)))
  }

  "sequence" should "return the first Left encountered when applied" in {
    assert(sequence(List(Right(1),Left("Error"),Right(3))) === Left("Error"))
  }

  it should "return a Right of all the values when no Lefts encountered" in {
    assert(sequence(List(Right(1),Right(2),Right(3))) === Right(List(1,2,3)))
  }

  // 4.8
  // map2 would need to return a list of all the Left values encountered
  // Would need a new data type since Either would just keep first value and not
  // accumulate.
}
