package com.ephox
package vault
package sql

import SqlT._
import ISqlT._
import java.sql.{ResultSetMetaData => R}
import scalaz._, Scalaz._

sealed trait ResultSetMetaData {
  private[sql] val x: java.sql.ResultSetMetaData

  import ResultSetMetaData._

  def catalogName(column: Int): Sql[String] =
    Try(x.getCatalogName(column))

  def columnClassName(column: Int): Sql[String] =
    Try(x.getColumnClassName(column))

  def columnCount: Sql[Int] =
    Try(x.getColumnCount)

  def columnDisplaySize(column: Int): Sql[Int] =
    Try(x.getColumnDisplaySize(column))

  def columnLabel(column: Int): Sql[String] =
    Try(x.getColumnLabel(column))

  def columnName(column: Int): Sql[String] =
    Try(x.getColumnName(column))

  def columnType(column: Int): ISql[SqlType] =
    Try(x.getColumnType(column)) ! (r =>
      SqlType.sqlTypeFromInt(r).toRightDisjunction(Incompatibility(r, "http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSetMetaData.html#getColumnType%28int%29", "Returns: SQL type from java.sql.Types")))

  def precision(column: Int): Sql[Int] =
    Try(x.getPrecision(column))

  def scale(column: Int): Sql[Int] =
    Try(x.getScale(column))

  def schemaName(column: Int): Sql[String] =
    Try(x.getSchemaName(column))

  def tableName(column: Int): Sql[String] =
    Try(x.getTableName(column))

  def isAutoIncrement(column: Int): Sql[Boolean] =
    Try(x.isAutoIncrement(column))

  def isCaseSensitive(column: Int): Sql[Boolean] =
    Try(x.isCaseSensitive(column))

  def isCurrency(column: Int): Sql[Boolean] =
    Try(x.isCurrency(column))

  def isDefinitelyWritable(column: Int): Sql[Boolean] =
    Try(x.isDefinitelyWritable(column))

  def isNullable(column: Int): ISql[ColumnNullability] =
    Try(x.isNullable(column)) ! (c =>
      if(c == R.columnNoNulls)
        ColumnNullability.NoNulls.right
      else if(c == R.columnNullable)
        ColumnNullability.Nullable.right
      else if(c == R.columnNullableUnknown)
        ColumnNullability.NullableUnknown.right
      else
        Incompatibility(c, """http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSetMetaData.html#isNullable%28int%29""", """Returns: the nullability status of the given column; one of columnNoNulls, columnNullable or columnNullableUnknown""").left
      )

  def isReadOnly(column: Int): Sql[Boolean] =
    Try(x.isReadOnly(column))

  def isSearchable(column: Int): Sql[Boolean] =
    Try(x.isSearchable(column))

  def isSigned(column: Int): Sql[Boolean] =
    Try(x.isSigned(column))

  def isWritable(column: Int): Sql[Boolean] =
    Try(x.isWritable(column))
}

object ResultSetMetaData {
  def apply(xx: java.sql.ResultSetMetaData): ResultSetMetaData =
    new ResultSetMetaData {
      val x = xx
    }

  sealed trait ColumnNullability

  object ColumnNullability {
    case object NoNulls extends ColumnNullability
    case object Nullable extends ColumnNullability
    case object NullableUnknown extends ColumnNullability
  }
}
