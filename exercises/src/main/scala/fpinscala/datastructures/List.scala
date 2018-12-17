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

  val a = List(1,2,3,4,5) match {
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
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, tlen) => 1 + tlen)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(accu: B, l: List[A]): B = l match {
      case Cons(h, t) => go(f(accu, h), t)
      case Nil => accu
    }
    go(z, l)
  }

  def sumLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def productLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def lengthLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((tlen, _) => tlen + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((rev, a) => Cons(a, rev))
  }

//  def foldLeftByFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
//    l match {
//      case Nil =>
//    }
//  }

  def foldRightByFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  def foldRightByFoldLeft2[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def concat[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil: List[A])(append)
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def mapBetter[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def filterBetter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*)
  }

  def flatMapMut[A, B](l: List[A])(f: A => List[B]): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf :+ f(h); go(t)
    }
    go(l)
    List(buf.toList: _*)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def zipAndAddRec(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAndAddRec(t1, t2))
  }

  def zipAndAdd(l1: List[Int], l2: List[Int]): List[Int] = {
    def go(l1: List[Int], l2: List[Int], accu: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => accu
      case (_, Nil) => accu
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(h1 + h2, accu))
    }
    go(l1, l2, Nil)
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    def go(l1: List[A], l2: List[B], accu: List[C]): List[C] = (l1, l2) match {
      case (Nil, _) => accu
      case (_, Nil) => accu
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(f(h1, h2), accu))
    }
    go(l1, l2, Nil)
  }

//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
//
//  }
}

object LeftTest {

  import List._

  def main(args: Array[String]): Unit = {
    val list1 = List(2, 4, 6, 8, 10)
    val list2 = List(2.0, 4.0, 6.0, 8.0)
    println(sumLeft(list1))
    println(productLeft(list2))
    println(lengthLeft(list1))
    println(lengthLeft(list2))
    println(reverse(list1))
    println(reverse(list2))
  }
}
