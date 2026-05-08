package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z

  def exists(p: A => Boolean): Boolean =
    // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => LazyList.cons(h(), t().take(n - 1))
    case _                   => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Empty               => Empty
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, acc) =>
      if p(a) then LazyList.cons(a, acc) else Empty
    )

  def forAll(p: A => Boolean): Boolean = this match
    case Empty      => true
    case Cons(h, t) => if p(h()) then t().forAll(p) else false

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, acc) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, acc) => LazyList.cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    this.foldRight(Empty: LazyList[A])((a, acc) =>
      if p(a) then LazyList.cons(a, acc) else acc
    )

  // this.append(other) = [this, other]
  def append[A2 >: A](other: => LazyList[A2]): LazyList[A2] =
    foldRight(other)(LazyList.cons(_, _))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, acc) => f(a).append(acc))

  // Note(rachel): the crazy syntax after unfold() is this:
  // https://scala-lang.org/files/archive/spec/3.4/08-pattern-matching.html#pattern-matching-anonymous-functions
  def mapViaUnfold[B](f: A => B): LazyList[B] = LazyList.unfold(this):
    case Empty      => None
    case Cons(h, t) => Some(f(h()), t())

  def takeViaUnfold(n: Int): LazyList[A] = LazyList.unfold((n, this)):
    case (nn, Cons(h, t)) if nn > 0 => Some(h(), (nn - 1, t()))
    case _                          => None

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = LazyList.unfold(this):
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _                    => None

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, other)):
      case (Cons(x, xs), Cons(y, ys)) => Some(f(x(), y()), (xs(), ys()))
      case _                          => None

  def zipAll[B](other: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, other)):
      case (Cons(x, xs), Cons(y, ys)) =>
        Some((Some(x()), Some(y())), (xs(), ys()))
      case (Cons(x, xs), Empty) =>
        Some((Some(x()), None), (xs(), Empty))
      case (Empty, Cons(y, ys)) =>
        Some((None, Some(y())), (Empty, ys()))
      case (Empty, Empty) => None

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).foldRight(true)((zipped, acc) =>
      zipped match
        case (None, Some(_))              => false
        case (Some(a), Some(b)) if a != b => false
        case _                            => acc
    )

  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(Some(this): Option[LazyList[A]]):
      case None                 => None
      case Some(Empty)          => Some(Empty, None)
      case Some(c @ Cons(h, t)) => Some(c, Some(t()))

  def hasSubsequence[A2 >: A](l: LazyList[A2]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] =
    foldRight((z, LazyList.cons(z, Empty))) { case (a, (acc, out)) =>
      lazy val b = f(a, acc)
      (b, LazyList.cons(b, out))
    }._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  def makeFibs(a: Int, b: Int): LazyList[Int] = cons(a, makeFibs(b, a + b))

  lazy val fibs: LazyList[Int] = makeFibs(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None         => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((a, b) => Some((a, (b, a + b))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(k => Some(k, k + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
