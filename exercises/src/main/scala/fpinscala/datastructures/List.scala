package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(h1, t1) => Cons(h, t1)
    case _ => Nil
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = 
    if (n<=0) l
    else drop(tail(l), n-1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t,f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def buildList(queue: List[A], acc: List[A]) : List[A] = queue match {
      case Cons(h,t) if (t!=Nil) => buildList(tail(queue), append(acc,List(h)))
      case _ => acc
    }

    buildList(l,Nil)
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_:A,x:Int) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Int]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + 1)

  def reverse[A](l: List[A]) : List[A] = 
    foldLeft(l, List[A]())((b,a) => Cons(a,b))

  def foldLeftViaRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(l, z)((b,a) => f(a,b))

  def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(as, z)((a,b) => f(b,a))

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)((a,b) => Cons(a,b))

  def concatList[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((b,a) => append(a,b))

  def addOneToList(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a,b) => Cons(a+1,b))

  def stringifyDoubleList(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a,b) => Cons(a.toString,b))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, List[B]())((a,b) => Cons(f(a),b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a,b) => if (f(a)) Cons(a,b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, List[B]())((b,a) => append(b, f(a)))

  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if (f(a)) List(a) else Nil)

  def addCorresponding(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (r, Nil) => r
    case (Nil, r) => r
    case (Cons(l1h, l1t), Cons(l2h, l2t)) => Cons(l1h+l2h, addCorresponding(l1t, l2t))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A)=>A): List[A] = (l1, l2) match {
    case (r, Nil) => r
    case (Nil, r) => r
    case (Cons(l1h, l1t), Cons(l2h, l2t)) => Cons(f(l1h,l2h), zipWith(l1t, l2t)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(sup1: List[A], sub2: List[A]): Boolean = (sup1, sub2) match {
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) if (h1==h2) => startsWith(t1,t2)
      case _ => false
    }

    sup match {
      case Nil => sub==Nil
      case s if startsWith(s, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
    }
  }
}
