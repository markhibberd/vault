package com.ephox
package vault
package sql

import SqlT._

sealed trait DatabaseMetaData {
  private[sql] val x: java.sql.DatabaseMetaData

  def allProceduresAreCallable: Sql[Boolean] =
    Try(x.allProceduresAreCallable)

  def allTablesAreSelectable: Sql[Boolean] =
    Try(x.allTablesAreSelectable)

  def dataDefinitionCausesTransactionCommit: Sql[Boolean] =
    Try(x.dataDefinitionCausesTransactionCommit)

  def dataDefinitionIgnoredInTransactions: Sql[Boolean] =
    Try(x.dataDefinitionIgnoredInTransactions)

  def deletesAreDetectable(t: ResultSetType): Sql[Boolean] =
    Try(x.deletesAreDetected(t.int))

  def doesMaxRowSizeIncludeBlobs: Sql[Boolean] =
    Try(x.doesMaxRowSizeIncludeBlobs)

  def getAttributes(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String, attributeNamePattern: String): Sql[ResultSet] =
    error("")
}

object DatabaseMetaData {
  def apply(xx: java.sql.DatabaseMetaData): DatabaseMetaData =
    new DatabaseMetaData {
      val x = xx
    }
}
