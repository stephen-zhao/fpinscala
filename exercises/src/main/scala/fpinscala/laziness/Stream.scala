package fpinscala.laziness

import Stream._

import scala.collection.mutable.ListBuffer
trait Stream[+A] {

  def toList: List[A] = {
    def loop(s: Stream[A], accu: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: accu)
      case _ => accu.reverse
    }
    loop(this, List())
  }

  def toListRec: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toListRec
      case _ => List.empty
    }

  def toListFast: List[A] = {
    val buf: ListBuffer[A] = new ListBuffer[A]
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        loop(t())
      case _ => buf.toList
    }
    loop(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

//  @annotation.tailrec
//  final def find(f: A => Boolean): Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => if (f(h())) Some(h()) else t() find f
//  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t() take (n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeTailRec(n: Int): Stream[A] = {
    val buf: ListBuffer[A] = new ListBuffer[A]
    def loop(s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 1 =>
        buf += h()
        loop(t(), n - 1)
      case Cons(h, _) if n == 1 =>
        buf += h()
        build(buf)
      case _ =>
        build(buf)
    }
    def build(l: ListBuffer[A]): Stream[A] =
      l.foldRight(Stream.empty[A])((a, s) => Stream.cons[A](a, s))

    loop(this, n)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop (n - 1)
    case _ if n == 0 => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
      val buf: ListBuffer[A] = new ListBuffer[A]
      def loop(s: Stream[A]): Stream[A] = s match {
        case c @ Cons(h, _) =>
          lazy val h_ = h()
          if (p(h_)) {
            buf += h_
            loop(c.t())
          }
          else {
            build(buf)
          }
        case _ =>
          build(buf)
      }
      def build(l: ListBuffer[A]): Stream[A] =
        l.foldRight(Stream.empty[A])((a, s) => Stream.cons[A](a, s))

      loop(this)
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty[A])
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h) append t)
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold[B, Stream[A]](this)({
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    })
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold[A, (Stream[A], Int)]((this, n))({
      case (Cons(h, t), n_) if n_ > 1 => Some((h(), (t(), n - 1)))
      case (Cons(h, _), 1) => Some((h(), (Empty, 0)))
      case _ => None
    })
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold[A, Stream[A]](this)({
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold[C, (Stream[A], Stream[B])]((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    })
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])](this, s2)({
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    })
  }

  def zipAllWith[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold[C, (Stream[A], Stream[B])](this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (Empty, t2()))
      case _ => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s) takeWhile {
      case (_, Some(_)) => true
      case _ => false
    } forAll {
      case (a, b) => a == b
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold[Stream[A], Stream[A]](this) {
      case s @ Cons(_, t) => Some(s, t())
      case _ => None
    }
  }

  def hasSubsequence[B](s: Stream[B]): Boolean = {
    tails exists (_ startsWith s)
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =  {
    foldRight[(B, Stream[B])]((z, Stream(z))) {
      case (h, (interim, tail)) =>
        lazy val evalTail = tail
        lazy val newInterim = f(h, interim)
        (newInterim, Stream.cons[B](newInterim, evalTail))
    }._2
  }

  override def toString: String = this match {
    case Cons(h, _) => s"Cons(${h()}, ?)"
    case Empty => "Empty"
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = Stream.cons[Int](n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      Stream.cons[Int](n1, go(n2, n1+n2))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, state)) => Stream.cons[A](h, unfold(state)(f))
    case None => Stream.empty[A]
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold[Int, (Int, Int)]((0, 1))({
      case (n1, n2) => Some((n1, (n2, n1 + n2)))
    })
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold[A, A](a)(_ => Some((a, a)))

  val onesViaUnfold: Stream[Int] = unfold[Int, Int](1)(_ => Some(1, 1))
  val onesViaConstant: Stream[Int] = constant(1)
}

object StreamTest {

  def pnr(n: Int): Int = {
    println(n)
    n
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3))
    println(Stream(1, 2, 3).tails)
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) hasSubsequence Stream(4, 5, 6))
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) hasSubsequence Stream(4, 7, 6))
    println(Stream(pnr(1), pnr(2), pnr(3), pnr(4), pnr(5), pnr(6), pnr(7)).scanRight(0)(_ + _).toList)
  }

}