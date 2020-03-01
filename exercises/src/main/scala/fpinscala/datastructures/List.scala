package fpinscala.datastructures

import scala.annotation.tailrec

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
    case Nil => Nil // new RuntimeException()
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil) // error
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil // error
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc) // foldLeft(l, 0)((acc, _) => 1 + acc)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumL(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productL(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => 1 + acc)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))

  def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))
  // one more foldRightL

  def appendR[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  def appendL[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((acc, x) => Cons(x, acc))

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(appendR)
//    foldLeft(ls, Nil: List[A])((acc, l) => appendL(acc, l))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightL(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightL(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightL(as, Nil: List[B])((x, acc) => appendR(f(x), acc))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

//  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
//    map(zip(as, bs))(p => f(p._1, p._2))

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons((ha, hb), zip(ta, tb))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val l = length(sub)

    def subseq(ssup: List[A]) = {
      val zs = zipWith(ssup, sub)(_ == _)
      if (length(zs) == l)
        foldRightL(zs, true)(_ && _)
      else
        false
    }

    @tailrec
    def iter(ssup: List[A]): Boolean = ssup match {
      case Nil => false
      case _ if (subseq(ssup)) => true
      case Cons(_, t) => iter(t)
    }

    iter(sup)
  }

}
