package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None 

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    this map (Some(_)) getOrElse ob 

  def filter(f: A => Boolean): Option[A] = 
    flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(ax), Some(bx)) => Some(f(ax, bx))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def foldFunc(b: Option[A], acc: Option[List[A]]) : Option[List[A]] = (acc, b) match {
      case (Some(acclist), Some(item)) => Some(item :: acclist)
      case _ => None
    }

    a.foldRight(Some(List[A]()) : Option[List[A]])(foldFunc)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def foldFunc(b: A, acc: Option[List[B]]) : Option[List[B]] = (f(b), acc) match {
      case (Some(item), Some(acclist)) => Some(item :: acclist)
      case _ => None
    }

    a.foldRight(Some(List[B]()) : Option[List[B]])(foldFunc)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }
}
