package com.ephox.vault

import scalaz._
import Scalaz._

object Query {
  def list[A](access: RowAccessor[A], sql: String) =
    (access -||> IterV.repeat[A, Option[A], List](IterV.head[A]) <|- sql.toSql) map (_.flatten)

  def first[A](access: RowAccessor[A], sql: SQLQuery) =
    access -||> IterV.head[A] <|- sql

  def byId[A](access: RowAccessor[A], sql: String, id: Long) =
    first(access, sql.bindSql(longType(id)))

  def listm[A](access: RowAccessor[A], sql: String)(implicit merge: Merger[A]) =
    access -||> combineAll <|- sql.toSql

  def byIdm[A](access: RowAccessor[A], sql: String, id: Long)(implicit merge: Merger[A]) =
    access -||> combine <|- sql.bindSql(longType(id))
}
