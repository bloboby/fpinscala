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

trait Prop

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

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
