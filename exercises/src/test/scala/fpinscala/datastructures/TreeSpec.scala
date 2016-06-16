package fpinscala.datastructures

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TreeSpec extends FlatSpec with Matchers {
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
    Tree.size(leaf1) shouldBe 1
  }

  it should "return 3 for branch1" in {
    Tree.size(branch1) shouldBe 3
  }

  it should "return 5 for branch2" in {
    Tree.size(branch2) shouldBe 5
  }

  it should "return 7 for branch3" in {
    Tree.size(branch3) shouldBe 7
  }

  //3.26
  "maximum" should "return the leaf value when given a leaf" in {
    maximum(leaf1) shouldBe leaf1.value
  }

  it should "return leaf2 for branch1" in {
    maximum(branch1) shouldBe leaf2.value
  }

  it should "return leaf4 for branch3" in {
    maximum(branch3) shouldBe leaf4.value
  }

  //3.27
  "depth" should "return 0 for a leaf" in {
    depth(leaf1) shouldBe 0
  }

  it should "return 1 for a branch with two leaves" in {
    depth(branch1) shouldBe 1
  }

  it should "return 3 for branch 3" in {
    depth(branch3) shouldBe 3
  }

  //3.28
  "map" should "add 1 to a leaf" in {
    map(leaf1)(_ + 1) shouldBe Leaf(2)
  }

  it should "add 1 to a branch" in {
    map(branch1)(_ + 1) shouldBe Branch(Leaf(2),Leaf(3))
  }

  //3.29
  "fold" should "have the same behaviour as size" in {
    sizeUsingFold(leaf1) shouldBe Tree.size(leaf1)
    sizeUsingFold(branch1) shouldBe Tree.size(branch1)
    sizeUsingFold(branch2) shouldBe Tree.size(branch2)
    sizeUsingFold(branch3) shouldBe Tree.size(branch3)
  }
  
  it should "have the same behaviour as maximum" in {
    maximumUsingFold(leaf1) shouldBe maximum(leaf1)
    maximumUsingFold(branch1) shouldBe maximum(branch1)
    maximumUsingFold(branch2) shouldBe maximum(branch2)
  }

  it should "have the same behaviour as depth" in {
    depthUsingFold(leaf1) shouldBe depth(leaf1)
    depthUsingFold(branch1) shouldBe depth(branch1)
    depthUsingFold(branch2) shouldBe depth(branch2)
  }

  it should "have the same behaviour as map" in {
    mapUsingFold(leaf1)(_.toString) shouldBe map(leaf1)(_.toString)
    mapUsingFold(branch1)(_.toString) shouldBe map(branch1)(_.toString)
  }
}
