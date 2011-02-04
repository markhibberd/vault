package com.ephox.vault2

import java.sql.PreparedStatement
import scalaz._
import Scalaz._

sealed trait PreparedStatementW {
  val s: PreparedStatement

  def tryExecuteUpdate: SQLValue[Int] =
    tryValue(s.executeUpdate)

  def executeStatements[T[_], A](as: T[A], k: A => Connector[Unit])(implicit f: Foldable[T]): Connector[Int] =
    connector(c => as.foldLeftM(0) {
       case (n, a) => {
         k(a)(c)
         s.tryExecuteUpdate ∘ (n+)
       }
     })

  def foreachStatement[T[_], A](as: T[A], k: A => Unit)(implicit f: Foldable[T]): Connector[Int] =
    executeStatements(as, (a: A) => k(a).η[Connector])

  def set(values: JDBCType*) =
    setValues(values.toList)

  def setValues[F[_]](values: F[JDBCType])(implicit fold: Foldable[F]) =
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

object PreparedStatementW {
  implicit def PreparedStatementPreparedStatementW(t: PreparedStatement): PreparedStatementW = new PreparedStatementW {
    val s = t
  }

  implicit def PreparedStatementWPreparedStatement(t: PreparedStatementW): PreparedStatement =
    t.s
}
