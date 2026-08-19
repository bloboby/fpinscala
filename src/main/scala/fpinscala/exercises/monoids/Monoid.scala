package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 + a2
    val empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 * a2
    val empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 || a2
    val empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 && a2
    val empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val empty = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(x: A => A, y: A => A): A => A = a => y(x(a))
    val empty = identity

  import fpinscala.exercises.testing.{Prop, Gen}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.empty)(m.combine)

  // TODO: verify this is correct. tests are insufficient.
  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid)(f.curried)(acc)

  // TODO: verify this is correct. tests are insufficient.
  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match
      case 0 => m.empty
      case 1 => f(as(0))
      case _ =>
        val (xs, ys) = as.splitAt(as.length / 2)
        val (x, y) = (foldMapV(xs, m)(f), foldMapV(ys, m)(f))
        m.combine(x, y)

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(x: Par[A], y: Par[A]): Par[A] = x.map2(y)(m.combine)
    val empty = Par.unit(m.empty)

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  opaque type Om = (Boolean, Option[(Int, Int)])
  def orderedM: Monoid[Om] = new:
    def combine(x: Om, y: Om): Om = (x, y) match
      case ((true, Some(a, b)), (true, Some(c, d))) if b <= c =>
        (true, Some(a, d))
      case (a, (true, None)) => a
      case ((true, None), b) => b
      case _                 => (false, None)

    val empty = (true, None)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedM)(a => (true, Some(a, a)))(0)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    def combine(a1: WC, a2: WC): WC = (a1, a2) match
      case (WC.Stub(a), WC.Stub(b))                   => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, n, r))             => WC.Part(a + l, n, r)
      case (WC.Part(l, n, r), WC.Stub(b))             => WC.Part(l, n, r + b)
      case (WC.Part(l1, n1, ""), WC.Part("", n2, r2)) =>
        WC.Part(l1, n1 + n2, r2)
      case (WC.Part(l1, n1, r1), WC.Part(l2, n2, r2)) =>
        WC.Part(l1, n1 + n2 + 1, r2)

    val empty = WC.Stub("")

  def count(s: String): Int =
    def f(x: String) = if x.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(a =>
      if a.isWhitespace then WC.Part("", 0, "") else WC.Stub(a.toString)
    ) match
      case WC.Stub(x)       => f(x)
      case WC.Part(l, n, r) => f(l) + n + f(r)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)]
  with
    def combine(x: (A, B), y: (A, B)) =
      (ma.combine(x(0), y(0)), mb.combine(x(1), y(1)))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty))
        )
    val empty = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    import Foldable.given
    given Monoid[Int] = intAddition
    as.foldMap(a => Map(a -> 1))

end Monoid
