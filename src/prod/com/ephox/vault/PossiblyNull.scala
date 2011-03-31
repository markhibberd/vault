package com.ephox.vault

import scalaz._, Scalaz._

// isomorphic to Option[A]
sealed trait PossiblyNull[A] {
  import PossiblyNull._
  import Key._

  def fold[X](nn: A => X, in: => X): X = this match {
    case NotNull(a) => nn(a)
    case IsNull()   => in
  }

  def ifelse[X](nn: => X, in: => X) =
    fold(_ => nn, in)

  def toOption: Option[A] =
    fold(Some(_), None)

  def toList: List[A] =
    fold(List(_), Nil)

  def map[B](f: A => B): PossiblyNull[B] =
    fold(notNull compose f, isNull)

  def flatMap[B](f: A => PossiblyNull[B]): PossiblyNull[B] =
    fold(f, isNull)

  def filter(p: A => Boolean): PossiblyNull[A] =
    fold(a => if(p(a)) this else isNull, isNull)

  def foreach(p: A => Unit) =
    fold(p, ())

  def ? =
    fold(_ => false, true)

  def !? =
    !this.?

  def |(a: => A): A =
    fold(identity, a)

  def exists(p: A => Boolean) =
    fold(p, false)

  def forall(p: A => Boolean) =
    fold(p, true)

  def toJDBCType(k: A => JDBCType): JDBCType =
    k(this | null.asInstanceOf[A])

  def toKey(implicit i: A <:< Long) =
    fold(a => key(a: Long), nokey)

  def orElse(n: => PossiblyNull[A]): PossiblyNull[A] =
    ifelse(this, n)

  def orElseValue(v: => A): PossiblyNull[A] =
    ifelse(this, notNull(v))
}
private case class NotNull[A](a: A) extends PossiblyNull[A]
private case class IsNull[A]() extends PossiblyNull[A]

object PossiblyNull extends PossiblyNulls

trait PossiblyNulls {
  def notNull[A]: A => PossiblyNull[A] = (a: A) => NotNull(a)
  def isNull[A]: PossiblyNull[A] = IsNull()
  def optionPossiblyNull[A](o: Option[A]): PossiblyNull[A] = o match {
    case Some(a) => notNull(a)
    case None    => isNull
  }


  implicit val PossiblyNullInjective = Injective[PossiblyNull]

  implicit val PossiblyNullFunctor: Functor[PossiblyNull] = new Functor[PossiblyNull] {
    def fmap[A, B](r: PossiblyNull[A], f: A => B) =
      r map f
  }

  implicit val PossiblyNullBind: Bind[PossiblyNull] = new Bind[PossiblyNull] {
    def bind[A, B](a: PossiblyNull[A], f: A => PossiblyNull[B]) =
      a flatMap f
  }

  implicit val PossiblyNullPure: Pure[PossiblyNull] = new Pure[PossiblyNull] {
    def pure[A](a: => A) = notNull(a)
  }

  implicit val PossiblyNullEach: Each[PossiblyNull] = new Each[PossiblyNull] {
    def each[A](e: PossiblyNull[A], f: A => Unit) =
      e foreach f
  }

  implicit val PossiblyNullIndex: Index[PossiblyNull] = new Index[PossiblyNull] {
    def index[A](a: PossiblyNull[A], i: Int) =
      a filter (_ => i == 0) toOption
  }

  implicit val PossiblyNullLength: Length[PossiblyNull] = new Length[PossiblyNull] {
    def len[A](a: PossiblyNull[A]) =
      a ifelse(1, 0)
  }

  implicit val PossiblyNullFoldable: Foldable[PossiblyNull] = new Foldable[PossiblyNull] {
    override def foldLeft[A, B](e: PossiblyNull[A], b: B, f: (B, A) => B) =
      e fold (f(b, _), b)

    override def foldRight[A, B](e: PossiblyNull[A], b: => B, f: (A, => B) => B) =
      e fold (f(_, b), b)
  }

  implicit val PossiblyNullTraverse: Traverse[PossiblyNull] = new Traverse[PossiblyNull] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: PossiblyNull[A]): F[PossiblyNull[B]] =
      as fold (a => f(a) ∘ (notNull(_: B)), isNull[B].pure[F])
  }

  implicit val PossiblyNullPlus: Plus[PossiblyNull] = new Plus[PossiblyNull] {
    def plus[A](a1: PossiblyNull[A], a2: => PossiblyNull[A]) =
      a1 orElse a2
  }

  implicit val PossiblyNullEmpty: Empty[PossiblyNull] = new Empty[PossiblyNull] {
    def empty[A] = isNull
  }

  implicit def PossiblyNullShow[A: Show]: Show[PossiblyNull[A]] = new Show[PossiblyNull[A]] {
    def show(a: PossiblyNull[A]) =
      a fold(
              a => ("not-null(" + a + ")")
            , ("is-null(" + a.shows + ")")
            ) toList
  }

  implicit def PossiblyNullEqual[A: Equal]: Equal[PossiblyNull[A]] =
    Equal.OptionEqual[A] ∙ (_.toOption)

  implicit def PossiblyNullOrder[A: Order]: Order[PossiblyNull[A]] =
    Order.OptionOrder[A] ∙ (_.toOption)

  implicit def PossiblyNullZero[A: Zero]: Zero[PossiblyNull[A]] =
    zero(notNull(∅[A]))
}