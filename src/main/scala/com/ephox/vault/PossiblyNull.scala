package com.ephox.vault

import scalaz._, Scalaz._

// isomorphic to Option[A]
sealed trait PossiblyNull[A] {
  import PossiblyNull._
  import CompanionKey._

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

  implicit val PossiblyNullEach: Each[PossiblyNull] = new Each[PossiblyNull] {
    def each[A](e: PossiblyNull[A])(f: A => Unit) =
      e foreach f
  }

  implicit val PossiblyNullIndex: Index[PossiblyNull] = new Index[PossiblyNull] {
    def index[A](a: PossiblyNull[A], i: Int) =
      a filter (_ => i == 0) toOption
  }

  implicit val PossiblyNullLength: Length[PossiblyNull] = new Length[PossiblyNull] {
    def length[A](a: PossiblyNull[A]) =
      a ifelse(1, 0)
  }

  implicit val PossiblyNullTraverse: Traverse[PossiblyNull] = new Traverse[PossiblyNull] {
    def traverseImpl[F[_] : Applicative, A, B](as: PossiblyNull[A])(f: A => F[B]): F[PossiblyNull[B]] =
      as fold (a => f(a) ∘ (notNull(_: B)), isNull[B].point[F])
  }

  implicit val PossiblyNullMonadPlus: MonadPlus[PossiblyNull] = new MonadPlus[PossiblyNull] {
    def plus[A](a1: PossiblyNull[A], a2: => PossiblyNull[A]) =
      a1 orElse a2

    def empty[A] = isNull

    def bind[A, B](a: PossiblyNull[A])(f: A => PossiblyNull[B]) =
      a flatMap f

    def point[A](a: => A) = notNull(a)
  }

  trait PossiblyNullEqual[A] extends Equal[PossiblyNull[A]] {
    implicit def A: Equal[A]

    override def equalIsNatural: Boolean = A.equalIsNatural

    override def equal(a1: PossiblyNull[A], a2: PossiblyNull[A]) =
      Equal.equalBy((_: PossiblyNull[A]).toOption) equal (a1, a2)
  }

  trait PossiblyNullOrder[A] extends Order[PossiblyNull[A]] {
    implicit def A: Order[A]

    override def order(a1: PossiblyNull[A], a2: PossiblyNull[A]) =
      Order.orderBy ((_: PossiblyNull[A]).toOption) apply (a1, a2)
  }

  implicit def PossiblyNullShow[A: Show]: Show[PossiblyNull[A]] = new Show[PossiblyNull[A]] {
    def show(a: PossiblyNull[A]) =
      a fold(
              a => "not-null(" + a.shows + ")"
            , "is-null"
            ) toList
  }

  implicit def PossiblyNullOrder[A](implicit A0: Order[A]): Order[PossiblyNull[A]] = new PossiblyNullOrder[A] {
    implicit def A = A0
  }

}
