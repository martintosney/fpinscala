package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(l: A=>B)(b: (B,B) => B): B = {
    def mapInner(t: Tree[A]): Tree[B] = t match {
      case Leaf(n) => Leaf(l(n))
      case Branch(left, right) => Branch(mapInner(left), mapInner(right))
    }

    def combineInner(t: Tree[B]): B = t match {
      case Leaf(n) => n
      case Branch(left, right) => b(combineInner(left), combineInner(right))
    }

    val mappedTree = mapInner(t)
    combineInner(mappedTree)
  }

  def sizeUsingFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(_ + _ + 1)

  def maximumUsingFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x: Int,y: Int) => (x max y)+1)

  def mapUsingFold[A,B](t: Tree[A])(f: A=>B) : Tree[B] =
    fold(t)(x=>Leaf(f(x)): Tree[B])(Branch(_,_))
}
