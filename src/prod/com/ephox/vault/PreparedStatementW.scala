package com.ephox.vault

import java.sql.PreparedStatement
import scalaz._
import Scalaz._

sealed trait PreparedStatementW {
  val s: PreparedStatement

  def tryExecuteUpdate[L]: SqlValue[L, Int] =
    trySqlValue(s.executeUpdate)

  // todo partially apply type parameters
  def executeStatements[L, T[_], A](as: T[A], k: A => SqlConnect[L, Unit])(implicit f: Foldable[T]): SqlConnect[L, Int] =
    sqlConnect[L, Int](c => as.foldLeftM[({type λ[α]= SqlValue[L, α]})#λ, Int](0) {
       case (n, a) => {
         k(a)(c)
         s.tryExecuteUpdate[L] map (n+)
       }
     })

  // todo partially apply type parameters
  def foreachStatement[L, T[_], A](as: T[A], k: A => Unit)(implicit f: Foldable[T]): SqlConnect[L, Int] =
    executeStatements(as, (a: A) => k(a).η[({type λ[α]= SqlConnect[L, α]})#λ])

  def set(values: JDBCType*): Unit =
    setValues(values.toList)

  def setValues[F[_]](values: F[JDBCType])(implicit fold: Foldable[F]): Unit =
    values.foldl(1) {
      case (n, v) => {
        v.fold(
                t => s.setNull(n, t.toType)
              , s.setBoolean(n, _)
              , s.setByte(n, _)
              , s.setShort(n, _)
              , s.setInt(n, _)
              , s.setLong(n, _)
              , s.setFloat(n, _)
              , s.setDouble(n, _)
              , s.setBigDecimal(n, _)
              , s.setString(n, _)
              , s.setBytes(n, _)
              , s.setDate(n, _)
              , s.setTime(n, _)
              , s.setTimestamp(n, _)
              , a => length => s.setAsciiStream(n, a, length)
              , a => length => s.setUnicodeStream(n, a, length)
              , a => length => s.setBinaryStream(n, a, length)
              , a => t => s.setObject(n, a, t.toType)
              , s.setObject(n, _)
              , a => length => s.setCharacterStream(n, a, length)
              , s.setRef(n, _)
              , s.setBlob(n, _)
              , s.setClob(n, _)
              , s.setArray(n, _)
              , a => cal => s.setDate(n, a, cal)
              , a => cal => s.setTime(n, a, cal)
              , a => cal => s.setTimestamp(n, a, cal)
              , t => m => s.setNull(n, t.toType, m)
              , s.setURL(n, _)
              )
        n + 1
      }
    }

}

trait PreparedStatementWs {
  implicit def PreparedStatementPreparedStatementW(t: PreparedStatement): PreparedStatementW = new PreparedStatementW {
    val s = t
  }

  implicit def PreparedStatementWPreparedStatement(t: PreparedStatementW): PreparedStatement =
    t.s
}
