package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case (l: Left[E]) => l
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => f(r)
   case (l: Left[EE]) => l
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case (r : Right[B]) => r
   case (_ : Left[EE]) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(a), Right(b)) => Right(f(a,b))
   case (l: Left[EE], _) => l
   case (_, r: Left[EE]) => r
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def foldFunc(b: A, acc: Either[E, List[B]]) : Either[E, List[B]] = (f(b), acc) match {
      case (Right(item), Right(acclist)) => Right(item :: acclist)
      case (l: Left[E], _) => l
      case (_, l: Left[E]) => l
    }

    es.foldRight(Right(List[B]()) : Either[E, List[B]])(foldFunc)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
