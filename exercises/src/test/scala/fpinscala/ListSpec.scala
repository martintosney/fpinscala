package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {
  import List._
  // 3.1
  "The expression in exercise 3.1" should "match the third case" in {
    assert(x===(1+2))
  }

  //3.2
  "tail" should "return Nil for an empty list" in {
    assert(tail(Nil)===Nil)
  }

  it should "return Nil for a list with one element" in {
    assert(tail(List(1))===Nil)
  }

  it should "return the list minus the first element for a larger list" in {
    assert(tail(List(1,2,3))===List(2,3))
  }

  //3.3
  "setHead" should "do nothing on an empty list" in {
    assert(setHead(Nil, 1)===Nil)
  }

  it should "replace the list for a list with one element" in {
    assert(setHead(List('a'),'b')===List('b'))
  }

  it should "replace the first element in a list for a longer list" in {
    assert(setHead(List('a','b','c'),'1')===List('1','b','c'))
  }

  //3.4
  val dropList = List(1,2,3,4)

  "drop" should "do nothing on an empty list" in {
    assert(drop(Nil, 1)===Nil)
  }

  it should "return the input list if i is zero" in {
    assert(drop(dropList,0)===dropList)
  }

  it should "remove the first two elements" in {
    assert(drop(dropList,2)===List(3,4))
  }

  //3.5
  def lessThan3(i: Int) = i < 3
  def dropList2 = List(4,3,2,1)

  "dropWhile" should "return the empty list for an empty list" in {
    assert(dropWhile(Nil,lessThan3)===Nil)
  }

  it should "not drop anything" in {
    assert(dropWhile(dropList2,lessThan3)===dropList2)
  }

  it should "drop the first two elements" in {
    assert(dropWhile(dropList,lessThan3)===List(3,4))
  }
  
  //3.6
  "init" should "return an empty list given an empty list" in {
    assert(init(Nil)===Nil)
  }

  it should "return a new list with all but the last element" in {
    assert(init(dropList)===List(1,2,3))
  }

  //3.7 - No, you can't short circuit.
  
  //3.8 
  "foldRight" should "with Cons should be equivalent to using List constructor" in {
    assert(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) === 
      List(1,2,3))
  }

  //3.9
  "length" should "should be zero for an empty list" in {
    assert(length(Nil)===0)
  }

  it should "should be four for a list of four elements" in {
    assert(length(dropList)===4)
  }

  //3.10
  "foldLeft" should "work" in {
    foldLeft(dropList, 0)((_,x) => x + 1)
  }

  //3.11
  "sumLeft" should "sum a list" in {
    assert(sumLeft(dropList)===10)
  }

  "productLeft" should "multiply a list" in {
    assert(productLeft(dropList)===24)
  }

  "lengthLeft" should "count elements in a list" in {
    assert(lengthLeft(dropList)===4)
  }

  //3.12
  val reverseList = List(1);
  "reverse" should "return the input list for Nil" in {
    assert(reverse(Nil)===Nil)
  }

  it should "return the input list for a single element list" in {
    assert(reverse(reverseList)===reverseList)
  }

  it should "reverse a list with more than one element" in {
    assert(reverse(dropList)===dropList2)
  }

  //3.13
  "foldLeftViaRight" should "give the same result as foldLeft" in {
    assert(foldLeftViaRight(dropList,0)(_ + _)===
      foldLeft(dropList,0)(_ + _))
  }

  "foldRightViaLeft" should "give the same result as foldRight" in {
    assert(foldRightViaLeft(dropList,0)(_ + _)===
      foldRight(dropList,0)(_ + _))
  }

  //3.14
  "appendFold" should "append two lists" in {
    assert(appendFold(List(1,2), List(3,4))===List(1,2,3,4))
  }

  //3.15
  "concatList" should "return Nil given an empty list" in {
    assert(concatList(Nil)===Nil)
  }

  it should "return Nil given a list containing an empty list" in {
    assert(concatList(List(Nil))===Nil)
  }

  it should "return Nil given a list containing two empty lists" in {
    assert(concatList(List(Nil, Nil))===Nil)
  }

  it should "return all elements for a list containing one list" in {
    assert(concatList(List(dropList))===dropList)
  }

  it should "return all elements for a list containing two lists" in {
    assert(concatList(List(dropList, dropList))===List(1,2,3,4,1,2,3,4))
  }
}
