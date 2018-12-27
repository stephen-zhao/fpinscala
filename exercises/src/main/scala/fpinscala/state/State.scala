package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, nextRNG) =>
      if (x < 0) (~x, nextRNG)
      else (x, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, nextRNG) => (n.toDouble / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = rng.nextInt match {
    case (n, rng1) => double(rng1) match {
      case (x, rng2) => ((n, x), rng2)
    }
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((n, x), rng1) => ((x, n), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = double(rng) match {
    case (n1, rng1) => double(rng1) match {
      case (n2, rng2) => double(rng2) match {
        case (n3, rng3) => ((n1, n2, n3), rng3)
      }
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, accu: (List[Int], RNG)): (List[Int], RNG) = {
      if (count == 0) accu
      else {
        val (x, rng1) = accu._2.nextInt
        go(count - 1, (x :: accu._1, rng1))
      }
    }
    go(count, (Nil, rng))
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(x => x.toDouble / (Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng0 => {
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDoubleViaBoth: Rand[(Int, Double)] = both(int, double)
  def doubleIntViaBoth: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight[Rand[List[A]]](unit(Nil)) {
    (h, accu) => map2(h, accu)(_ :: _)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence[Int](List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(x => {
    val mod = x % n
    if (x - mod + (n - 1) >= 0) unit(mod)
    else nonNegativeLessThan(n)
  })

  def mapViaFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] = flatMap(f)(a => {
    unit(g(a))
  })

  def map2ViaFlatMap[A, B, C](f1: Rand[A], f2: Rand[B])(g: (A, B) => C): Rand[C] = for {
    a <- f1
    b <- f2
  } yield g(a, b)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s0 => {
    val (a, s1) = run(s0)
    (f(a), s1)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s0 => {
    val (a, s1) = this.run(s0)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s0 => {
    val (a, s1) = this.run(s0)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight[State[S, List[A]]](unit(Nil)) {
    case (h, accu) => h.map2(accu)(_ :: _)
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def update(input: Input): Machine => Machine = {
    case machine @ Machine(locked, candies, coins) => input match {
      case Coin if locked && candies > 0 =>
        Machine(locked=false, candies, coins + 1)
      case Turn if !locked =>
        Machine(locked=true, candies - 1, coins)
      case _ => machine
    }
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (i => modify(update(i))))
    s <- get
  } yield (s.coins, s.candies)
}
