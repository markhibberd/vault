package com.ephox.vault

import java.sql.{Connection, Statement, PreparedStatement, SQLException}

// FIX sort this mess out
object StatementBinder {
  def statement[A](tx: Connection, sql: String, params: List[_], flags: Int = Statement.NO_GENERATED_KEYS)(f: PreparedStatement => A): Either[SQLException, A] = {
    try {
      val stmt = tx.prepareStatement(sql, flags)
      try {
        val meta = stmt.getParameterMetaData
        val count = meta.getParameterCount
        if (count != params.size)
          Left(new SQLException("Incorrect number of bind parameters for statemt. Sql[" + sql + "], Required params[" + count + "], Actual params[" + params.size + "]."))
        else {
          params.zipWithIndex.foreach {
            case (p, i) => stmt.setObject(i + 1, p)
          }
          Right(f(stmt))
        }
      } finally {
        stmt.close
      }
    } catch {
      case e: SQLException => Left(e)
    }
  }
}