package com.ephox.vault

import scalaz._, Scalaz._, iteratee._

object Queries {
  import StringQuery._
  import VaultIteratee._
  import JDBCType._

  def list[A](access: RowAccess[A], sql: Sql): RowConnect[List[A]] =
    (access -||> Iteratee.reversed[A, List] <|- sql) map (_.reverse)

  def first[A](access: RowAccess[A], sql: Sql) =
    access -||> Iteratee.head[A, Id] <|- sql

  def byId[A](access: RowAccess[A], sql: String, id: Long) =
    first(access, sql.bindValues(longType(id)))

  def listIds[A](access: RowAccess[A], sql: Sql, keys: List[Key])(implicit merge: Merger[A], keyed: Keyed[A]) = 
    list[A](access, sql) map (as => keys.map(key => 
        as.filter(keyed.get(_) == key).reduceLeft((acc, a) => merge.mergeOrFirst(acc, a))
    ))

  def listm[A](access: RowAccess[A], sql: Sql)(implicit merge: Merger[A]) =
    access -||> combineAll <|- sql

  def firstm[A](access: RowAccess[A], sql: Sql)(implicit merge: Merger[A]) =
    access -||> combine <|- sql

  def byIdm[A](access: RowAccess[A], sql: String, id: Long)(implicit merge: Merger[A]) =
    firstm(access,  sql.bindValues(longType(id)))
}
