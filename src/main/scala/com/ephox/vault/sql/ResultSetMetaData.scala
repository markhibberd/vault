package com.ephox
package vault
package sql

import SqlT._
import java.sql.{ResultSetMetaData => R}

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

  def columnType(column: Int): Sql[SqlType] =
    Try({
      val r = x.getColumnType(column)
      SqlType.sqlTypeFromIntOr(r, sys.error("[" + r + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSetMetaData.html#getColumnType%28int%29 Returns: SQL type from java.sql.Types"""))
    })

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

  def isNullable(column: Int): Sql[ColumnNullability] =
    Try(x.isNullable(column)) map (c =>
      if(c == R.columnNoNulls)
        ColumnNullability.NoNulls
      else if(c == R.columnNullable)
        ColumnNullability.Nullable
      else if(c == R.columnNullableUnknown)
        ColumnNullability.NullableUnknown
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSetMetaData.html#isNullable%28int%29 Returns: the nullability status of the given column; one of columnNoNulls, columnNullable or columnNullableUnknown""")
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
