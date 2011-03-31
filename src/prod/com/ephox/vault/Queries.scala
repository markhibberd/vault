package com.ephox.vault

import scalaz._
import Scalaz._

object Queries {
  import StringQuery._
  import VaultIteratee._
  import JDBCType._

  def list[L, A](access: RowAccess[L, A], sql: String) =
    (access -||> IterV.repeat[A, Option[A], List](IterV.head[A]) <|- sql.toSql) map (_.flatten)

  def first[L, A](access: RowAccess[L, A], sql: Query) =
    access -||> IterV.head[A] <|- sql

  def byId[L, A](access: RowAccess[L, A], sql: String, id: Long) =
    first(access, sql.bindSql(longType(id)))

  def listm[L, A](access: RowAccess[L, A], sql: String)(implicit merge: Merger[A]) =
    access -||> combineAll <|- sql.toSql

  def byIdm[L, A](access: RowAccess[L, A], sql: String, id: Long)(implicit merge: Merger[A]) =
    access -||> combine <|- sql.bindSql(longType(id))
}
