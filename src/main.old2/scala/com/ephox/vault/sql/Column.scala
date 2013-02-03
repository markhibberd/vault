package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait Column {
  import Column._

  def fold[X](index: Int => X, name: String => X): X =
    this match {
      case Index(n) => index(n)
      case Name(s) => name(s)
    }

  def withIndex(k: Int => Int): Column =
    indexL.mod(k, this)

  def withName(k: String => String): Column =
    nameL.mod(k, this)

  def ++ : Column =
    withIndex(1+)

  def -- : Column =
    withIndex(_-1)

  def index: Option[Int] =
    indexL.get(this)

  def name: Option[String] =
    nameL.get(this)

  def trim: Column =
    withName(_.trim)
}

object Column extends ColumnFunctions {
  private[sql] case class Index(n: Int) extends Column
  private[sql] case class Name(s: String) extends Column

  def apply(n: Int): Column =
    index(n)

  def index(n: Int): Column =
    Index(n)

  def name(s: String): Column =
    Name(s)

  def indexL: Column @?> Int =
    PLens {
      case Index(n) => Some(Store(Index(_), n))
      case Name(_) => None
    }

  def nameL: Column @?> String =
    PLens {
      case Index(_) => None
      case Name(s) => Some(Store(Name(_), s))
    }
}

trait ColumnFunctions {
  object ColumnName {
    def apply(s: String): Column =
      Column.Name(s)
  }

  object ColumnIndex {
    def apply(n: Int): Column =
      Column.Index(n)
  }

}
