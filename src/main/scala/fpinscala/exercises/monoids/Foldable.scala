package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      ???

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldRight(mb.empty)((a, b) => mb.combine(f(a), b))

    def combineAll(using ma: Monoid[A]): A =
      as.foldRight(ma.empty)(ma.combine)

    def toList: List[A] =
      foldRight(List[A]())((x, acc) => x :: acc)

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] = as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) = as match
        case Leaf(a)      => f(a, acc)
        case Branch(l, r) => l.foldRight(r.foldRight(acc)(f))(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
        case Leaf(a)      => f(acc, a)
        case Branch(l, r) => l.foldLeft(r.foldLeft(acc)(f))(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        if as.isEmpty then acc else f(as.get, acc)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        if as.isEmpty then acc else f(acc, as.get)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        if as.isEmpty then mb.empty else f(as.get)
