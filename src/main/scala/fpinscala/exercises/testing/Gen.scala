package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed          => false
      case Falsified(_, _) => true
      case Proved          => false

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

    def &&(that: Prop): Prop = (m, n, rng) =>
      (self(m, n, rng), that(m, n, rng)) match
        // TODO: preserve info about which case was falsified
        case (Result.Falsified(f, s), _) => Result.Falsified(f, s)
        case (_, Result.Falsified(f, s)) => Result.Falsified(f, s)
        case _                           => Result.Passed

    def ||(that: Prop): Prop = (m, n, rng) =>
      (self(m, n, rng), that(m, n, rng)) match
        // TODO: preserve info about which case was falsified
        case (Result.Falsified(f, s), Result.Falsified(_, _)) =>
          Result.Falsified(f, s)
        case _ => Result.Passed

opaque type Gen[+A] = State[RNG, A]

object Gen:

  def unit[A](a: => A): Gen[A] = State.unit(a)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def boolean: Gen[Boolean] =
    State(RNG.nonNegativeInt).map(n => n % 2 == 0)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val max = 10000
    val threshold = max * g1(1) / (g1(1) + g2(1))
    Gen.choose(0, max).flatMap(n => if n >= threshold then g2(0) else g1(0))

  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)

    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

trait SGen[+A]
