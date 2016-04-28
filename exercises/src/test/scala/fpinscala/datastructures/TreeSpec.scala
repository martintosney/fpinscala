package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {
  import Tree._

  val leaf1 = Leaf(1);
  val leaf2 = Leaf(2);
  val leaf3 = Leaf(3);
  val leaf4 = Leaf(4);
  val branch1 = Branch(leaf1, leaf2);
  val branch2 = Branch(branch1, leaf3);
  val branch3 = Branch(leaf4, branch2);

  //3.25
  "size" should "return 1 given a leaf" in {
    assert(size(leaf1)===1)
  }

  it should "return 3 for branch1" in {
    assert(size(branch1)===3)
  }

  it should "return 5 for branch2" in {
    assert(size(branch2)===5)
  }

  it should "return 7 for branch3" in {
    assert(size(branch3)===7)
  }

  //3.26
  "maximum" should "return the leaf value when given a leaf" in {
    assert(maximum(leaf1)===leaf1.value)
  }

  it should "return leaf2 for branch1" in {
    assert(maximum(branch1)===leaf2.value)
  }

  it should "return leaf4 for branch3" in {
    assert(maximum(branch3)===leaf4.value)
  }

  //3.27
  "depth" should "return 0 for a leaf" in {
    assert(depth(leaf1)===0)
  }

  it should "return 1 for a branch with two leaves" in {
    assert(depth(branch1)===1)
  }

  it should "return 3 for branch 3" in {
    assert(depth(branch3)===3)
  }

  //3.28
  "map" should "add 1 to a leaf" in {
    assert(map(leaf1)(_ + 1)===Leaf(2))
  }

  it should "add 1 to a branch" in {
    assert(map(branch1)(_ + 1)===Branch(Leaf(2),Leaf(3)))
  }

  //3.29
  "fold" should "have the same behaviour as size" in {
    assert(sizeUsingFold(leaf1)===size(leaf1))
    assert(sizeUsingFold(branch1)===size(branch1))
    assert(sizeUsingFold(branch2)===size(branch2))
    assert(sizeUsingFold(branch3)===size(branch3))
  }
  
  it should "have the same behaviour as maximum" in {
    assert(maximumUsingFold(leaf1)===maximum(leaf1))
    assert(maximumUsingFold(branch1)===maximum(branch1))
    assert(maximumUsingFold(branch2)===maximum(branch2))
  }

  it should "have the same behaviour as depth" in {
    assert(depthUsingFold(leaf1)===depth(leaf1))
    assert(depthUsingFold(branch1)===depth(branch1))
    assert(depthUsingFold(branch2)===depth(branch2))
  }

  it should "have the same behaviour as map" in {
    assert(mapUsingFold(leaf1)(_.toString)===map(leaf1)(_.toString))
    assert(mapUsingFold(branch1)(_.toString)===map(branch1)(_.toString))
  }
}
