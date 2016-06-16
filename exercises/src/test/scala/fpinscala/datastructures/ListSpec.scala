package fpinscala.datastructures

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ListSpec extends FlatSpec with Matchers {
  import List._

  // 3.1
  "The expression in exercise 3.1" should "match the third case" in {
    // assert(x===(1+2))
    x shouldBe (1+2)
  }

  //3.2
  "tail" should "return Nil for an empty list" in {
    tail(Nil) shouldBe Nil
  }

  it should "return Nil for a list with one element" in {
    tail(List(1)) shouldBe Nil
  }

  it should "return the list minus the first element for a larger list" in {
    tail(List(1,2,3)) shouldBe List(2,3)
  }

  //3.3
  "setHead" should "do nothing on an empty list" in {
    setHead(Nil, 1) shouldBe Nil
  }

  it should "replace the list for a list with one element" in {
    setHead(List('a'),'b') shouldBe List('b')
  }

  it should "replace the first element in a list for a longer list" in {
    setHead(List('a','b','c'),'1') shouldBe List('1','b','c')
  }

  //3.4
  val dropList = List(1,2,3,4)

  "drop" should "do nothing on an empty list" in {
    drop(Nil, 1) shouldBe Nil
  }

  it should "return the input list if i is zero" in {
    drop(dropList,0) shouldBe dropList
  }

  it should "remove the first two elements" in {
    drop(dropList,2) shouldBe List(3,4)
  }

  //3.5
  def lessThan3(i: Int) = i < 3
  def dropList2 = List(4,3,2,1)

  "dropWhile" should "return the empty list for an empty list" in {
    dropWhile(Nil,lessThan3) shouldBe Nil
  }

  it should "not drop anything" in {
    dropWhile(dropList2,lessThan3) shouldBe dropList2
  }

  it should "drop the first two elements" in {
    dropWhile(dropList,lessThan3) shouldBe List(3,4)
  }
  
  //3.6
  "init" should "return an empty list given an empty list" in {
    init(Nil) shouldBe Nil
  }

  it should "return a new list with all but the last element" in {
    init(dropList) shouldBe List(1,2,3)
  }

  //3.7 - No, you can't short circuit.
  
  //3.8 
  "foldRight" should "with Cons should be equivalent to using List constructor" in {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldBe List(1,2,3)
  }

  //3.9
  "length" should "should be zero for an empty list" in {
    List.length(Nil) shouldBe 0
  }

  it should "should be four for a list of four elements" in {
    List.length(dropList) shouldBe 4
  }

  //3.10
  "foldLeft" should "work" in {
    foldLeft(dropList, 0)((_,x) => x + 1)
  }

  //3.11
  "sumLeft" should "sum a list" in {
    sumLeft(dropList) shouldBe 10
  }

  "productLeft" should "multiply a list" in {
    productLeft(dropList) shouldBe 24
  }

  "lengthLeft" should "count elements in a list" in {
    lengthLeft(dropList) shouldBe 4
  }

  //3.12
  val reverseList = List(1);
  "reverse" should "return the input list for Nil" in {
    reverse(Nil) shouldBe Nil
  }

  it should "return the input list for a single element list" in {
    reverse(reverseList) shouldBe reverseList
  }

  it should "reverse a list with more than one element" in {
    reverse(dropList) shouldBe dropList2
  }

  //3.13
  "foldLeftViaRight" should "give the same result as foldLeft" in {
    foldLeftViaRight(dropList,0)(_ + _) shouldBe 
      foldLeft(dropList,0)(_ + _)
  }

  "foldRightViaLeft" should "give the same result as foldRight" in {
    foldRightViaLeft(dropList,0)(_ + _) shouldBe 
      foldRight(dropList,0)(_ + _)
  }

  //3.14
  "appendFold" should "append two lists" in {
    appendFold(List(1,2), List(3,4)) shouldBe List(1,2,3,4)
  }

  //3.15
  "concatList" should "return Nil given an empty list" in {
    concatList(Nil) shouldBe Nil
  }

  it should "return Nil given a list containing an empty list" in {
    concatList(List(Nil)) shouldBe Nil
  }

  it should "return Nil given a list containing two empty lists" in {
    concatList(List(Nil, Nil)) shouldBe Nil
  }

  it should "return all elements for a list containing one list" in {
    concatList(List(dropList)) shouldBe dropList
  }

  it should "return all elements for a list containing two lists" in {
    concatList(List(dropList, dropList)) shouldBe List(1,2,3,4,1,2,3,4)
  }

  //3.16
  "addOneToList" should "return Nil given an empty list" in {
    addOneToList(Nil) shouldBe Nil
  }

  it should "add 1 to each element in a non-empty list" in {
    addOneToList(dropList) shouldBe List(2,3,4,5)
  }

  //3.17
  val doubleList = List(1.0, 2.0)
  "stringifyDoubleList" should "return Nil given an empty list" in {
    stringifyDoubleList(Nil) shouldBe Nil
  }

  it should "turn doubles into strings" in {
    stringifyDoubleList(doubleList) shouldBe List("1.0", "2.0")
  }

  //3.18
  "map" should "have the same behaviour as addOneToList" in {
    map(dropList)(_ + 1) shouldBe addOneToList(dropList)
  }

  it should "have the same behaviour as stringifyDoubleList" in {
    map(doubleList)(_.toString) shouldBe stringifyDoubleList(doubleList)
  }

  //3.19
  "filter" should "return the input list given a non-exlusion filter" in {
    filter(dropList)(_ => true) shouldBe dropList
  }

  it should "return nothing given a complete exclusion filter" in {
    filter(dropList)(_ => false) shouldBe Nil
  }

  it should "return elements greater than two" in {
    filter(dropList)(_ > 2) shouldBe List(3,4)
  }

  //3.20
  "flatMap" should "return Nil given an input list of Nil" in {
    flatMap(Nil)(i => List(i,i)) shouldBe Nil
  }

  it should "return a single list" in {
    flatMap(dropList)(i => List(i,i)) shouldBe List(1,1,2,2,3,3,4,4)
  }

  //3.21
  "flatMapFilter" should "return the same result as Filter" in {
    flatMapFilter(dropList)(_ => true) shouldBe filter(dropList)(_ => true)
    flatMapFilter(dropList)(_ => false) shouldBe filter(dropList)(_ => false)
    flatMapFilter(dropList)(_ > 2) shouldBe filter(dropList)(_ > 2)
  }

  //3.22
  val corList1 = List(1, 3, 5)
  val corList2 = List(2, 4, 6)
  val corList3 = List(3, 5)
  "addCorresponding" should "work for two same-sized lists" in {
    addCorresponding(corList1, corList2) shouldBe List(3,7,11)
  }

  it should "work for lists of different sizes" in {
    addCorresponding(corList1, corList3) shouldBe List(4,8,5)
  }

  //3.23
  def zipAdd(a: Int, b: Int): Int = a + b
  "zipWith" should "behave the same as addCorresponding" in {
    zipWith(corList1, corList2)(zipAdd) shouldBe addCorresponding(corList1, corList2)
    zipWith(corList1, corList3)(zipAdd) shouldBe addCorresponding(corList1, corList3)
  }

  //3.24
  "hasSubsequence" should "detect the subsequence" in {
    assert(hasSubsequence(dropList,Nil))
    assert(hasSubsequence(dropList,List(1)))
    assert(hasSubsequence(dropList,List(2)))
    assert(hasSubsequence(dropList,List(3)))
    assert(hasSubsequence(dropList,List(4)))
    assert(hasSubsequence(dropList,List(1,2)))
    assert(hasSubsequence(dropList,List(2,3)))
    assert(hasSubsequence(dropList,List(3,4)))
    assert(hasSubsequence(dropList,List(1,2,3)))
    assert(hasSubsequence(dropList,List(2,3,4)))
    assert(hasSubsequence(dropList,List(1,2,3,4)))
  }

  it should "return false where no subsequence" in {
    assert(!hasSubsequence(dropList, List(2,4)))
  }

}
