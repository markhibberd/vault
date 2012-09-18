package com.ephox
package vault
package sql

import SqlT._
import XSqlT._
import java.sql.{ DatabaseMetaData => D, Connection => C, ResultSet => R }
import sql.Connection.ResultSetHoldability
import scalaz.Digit._0
import sql.XSqlT.TryNullT

sealed trait DatabaseMetaData {
  private[sql] val x: D

  import DatabaseMetaData._

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

  def attributes(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String, attributeNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getAttributes(catalog.orNull, schemaPattern.orNull, typeNamePattern, attributeNamePattern)))

  def bestRowIdentifier(catalog: Option[String], schema: Option[String], table: String, scope: BestRow, nullable: Boolean): Sql[ResultSet] =
    Try(ResultSet(x.getBestRowIdentifier(catalog.orNull, schema.orNull, table, scope.int, nullable)))

  def catalogs: Sql[ResultSet] =
    Try(ResultSet(x.getCatalogs))

  def catalogSeparator: Sql[String] =
    Try(x.getCatalogSeparator)

  def catalogTerm: Sql[String] =
    Try(x.getCatalogTerm)

  def columnPrivileges(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String, columnNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getColumnPrivileges(catalog.orNull, schemaPattern.orNull, typeNamePattern, columnNamePattern)))

  def columns(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String, columnNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getColumns(catalog.orNull, schemaPattern.orNull, typeNamePattern, columnNamePattern)))

  def connection: Sql[Connection] =
    Try(Connection(x.getConnection))

  def crossReference(primaryCatalog: Option[String], primarySchema: Option[String], primaryTable: String, foreignCatalog: Option[String], foreignSchema: Option[String], foreignTable: String): Sql[ResultSet] =
    Try(ResultSet(x.getCrossReference(primaryCatalog.orNull, primarySchema.orNull, primaryTable, foreignCatalog.orNull, foreignSchema.orNull, foreignTable)))

  def databaseMajorVersion: Sql[Int] =
    Try(x.getDatabaseMajorVersion)

  def databaseMinorVersion: Sql[Int] =
    Try(x.getDatabaseMinorVersion)

  def databaseProductName: Sql[String] =
    Try(x.getDatabaseProductName)

  def databaseProductVersion: Sql[String] =
    Try(x.getDatabaseProductVersion)

  def defaultTransactionIsolation: Sql[TransactionIsolation] =
    Try(x.getDefaultTransactionIsolation) map (c =>
      if(c == C.TRANSACTION_READ_UNCOMMITTED)
        TransactionIsolation.ReadUncommitted
      else if(c == C.TRANSACTION_READ_COMMITTED)
        TransactionIsolation.ReadCommitted
      else if(c == C.TRANSACTION_REPEATABLE_READ)
        TransactionIsolation.RepeatableRead
      else if(c == C.TRANSACTION_SERIALIZABLE)
        TransactionIsolation.Serializable
      else if(c == C.TRANSACTION_NONE)
        TransactionIsolation.None
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/DatabaseMetaData.html#getDefaultTransactionIsolation%28%29 Retrieves this database's default transaction isolation level. The possible values are defined in java.sql.Connection. """))

  def driverMajorVersion: Sql[Int] =
    Try(x.getDriverMajorVersion)

  def driverMinorVersion: Sql[Int] =
    Try(x.getDriverMinorVersion)

  def driverName: Sql[String] =
    Try(x.getDriverName)

  def driverVersion: Sql[String] =
    Try(x.getDriverVersion)

  def exportedKeys(catalog: Option[String], schema: Option[String], table: String): Sql[ResultSet] =
    Try(ResultSet(x.getExportedKeys(catalog.orNull, schema.orNull, table)))

  def extraNameCharacters: Sql[String] =
    Try(x.getExtraNameCharacters)

  def identifierQuoteString: Sql[String] =
    Try(x.getIdentifierQuoteString)

  def importedKeys(catalog: Option[String], schema: Option[String], table: String): Sql[ResultSet] =
    Try(ResultSet(x.getImportedKeys(catalog.orNull, schema.orNull, table)))

  def indexInfo(catalog: Option[String], schema: Option[String], table: String, unique: Boolean, approximate: Boolean): Sql[ResultSet] =
    Try(ResultSet(x.getIndexInfo(catalog.orNull, schema.orNull, table, unique, approximate)))

  def jdbcMajorVersion: Sql[Int] =
    Try(x.getJDBCMajorVersion)

  def jdbcMinorVersion: Sql[Int] =
    Try(x.getJDBCMinorVersion)

  def maxBinaryLiteralLength: Sql[Int] =
    Try(x.getMaxBinaryLiteralLength)

  def maxCatalogNameLength: Sql[Int] =
    Try(x.getMaxCatalogNameLength)

  def maxCharLiteralLength: Sql[Int] =
    Try(x.getMaxCharLiteralLength)

  def maxColumnNameLength: Sql[Int] =
    Try(x.getMaxColumnNameLength)

  def maxColumnsInGroupBy: Sql[Int] =
    Try(x.getMaxColumnsInGroupBy)

  def maxColumnsInIndex: Sql[Int] =
    Try(x.getMaxColumnsInIndex)

  def maxColumnsInOrderBy: Sql[Int] =
    Try(x.getMaxColumnsInOrderBy)

  def maxColumnsInSelect: Sql[Int] =
    Try(x.getMaxColumnsInSelect)

  def maxColumnsInTable: Sql[Int] =
    Try(x.getMaxColumnsInTable)

  def maxConnections: Sql[Int] =
    Try(x.getMaxConnections)

  def maxCursorNameLength: Sql[Int] =
    Try(x.getMaxCursorNameLength)

  def maxIndexLength: Sql[Int] =
    Try(x.getMaxIndexLength)

  def maxProcedureNameLength: Sql[Int] =
    Try(x.getMaxProcedureNameLength)

  def maxRowSize: Sql[Int] =
    Try(x.getMaxRowSize)

  def maxSchemaNameLength: Sql[Int] =
    Try(x.getMaxSchemaNameLength)

  def maxStatementLength: Sql[Int] =
    Try(x.getMaxStatementLength)

  def maxStatements: Sql[Int] =
    Try(x.getMaxStatements)

  def maxTableNameLength: Sql[Int] =
    Try(x.getMaxTableNameLength)

  def maxTablesInSelect: Sql[Int] =
    Try(x.getMaxTablesInSelect)

  def maxUserNameLength: Sql[Int] =
    Try(x.getMaxUserNameLength)

  def numericFunctions: Sql[String] =
    Try(x.getNumericFunctions)

  def primaryKeys(catalog: Option[String], schema: Option[String], table: String): Sql[ResultSet] =
    Try(ResultSet(x.getPrimaryKeys(catalog.orNull, schema.orNull, table)))

  def procedureColumns(catalog: Option[String], schemaPattern: Option[String], procedureNamePattern: String, columnNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getProcedureColumns(catalog.orNull, schemaPattern.orNull, procedureNamePattern, columnNamePattern)))

  def procedures(catalog: Option[String], schemaPattern: Option[String], procedureNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getProcedures(catalog.orNull, schemaPattern.orNull, procedureNamePattern)))

  def procedureTerm: Sql[String] =
    Try(x.getProcedureTerm)

  def resultSetHoldability: Sql[ResultSetHoldability] =
    Try(x.getResultSetHoldability) map (c =>
      if(c == R.HOLD_CURSORS_OVER_COMMIT)
        ResultSetHoldability.HoldsCursorsOverCommit
      else if(c == R.CLOSE_CURSORS_AT_COMMIT)
        ResultSetHoldability.CloseCursorsAtCommit
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/DatabaseMetaData.html#getResultSetHoldability%28%29 Returns: the default holdability; either ResultSet.HOLD_CURSORS_OVER_COMMIT or ResultSet.CLOSE_CURSORS_AT_COMMIT"""))

  def schemas: Sql[ResultSet] =
    Try(ResultSet(x.getSchemas))

  def schemaTerm: Sql[String] =
    Try(x.getSchemaTerm)

  def searchStringEscape: Sql[String] =
    Try(x.getSearchStringEscape)

  def sqlKeywords: Sql[String] =
    Try(x.getSQLKeywords)

  def sqlStateType: Sql[SqlStateType] =
    Try(x.getSQLStateType) map (c =>
      if(c == D.sqlStateXOpen)
        SqlStateType.SqlXOpen
      else if(c == D.sqlStateSQL99)
        SqlStateType.Sql99
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/DatabaseMetaData.html#getSQLStateType%28%29 Returns: the type of SQLSTATE; one of: sqlStateXOpen or sqlStateSQL99"""))

  def stringFunctions: Sql[String] =
    Try(x.getStringFunctions)

  def superTables(catalog: Option[String], schemaPattern: Option[String], tableNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getSuperTables(catalog.orNull, schemaPattern.orNull, tableNamePattern)))

  def superTypes(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getSuperTypes(catalog.orNull, schemaPattern.orNull, typeNamePattern)))

  def systemFunctions: Sql[String] =
    Try(x.getSystemFunctions)

  def tablePrivileges(catalog: Option[String], schemaPattern: Option[String], tableNamePattern: String): Sql[ResultSet] =
    Try(ResultSet(x.getTablePrivileges(catalog.orNull, schemaPattern.orNull, tableNamePattern)))

  def tables(catalog: Option[String], schemaPattern: Option[String], tableNamePattern: String, types: Option[List[String]]): Sql[ResultSet] =
    Try(ResultSet(x.getTables(catalog.orNull, schemaPattern.orNull, tableNamePattern, types map (_.toArray) orNull)))

  def tableTypes: Sql[ResultSet] =
    Try(ResultSet(x.getTableTypes))

  def timeDateFunctions: Sql[String] =
    Try(x.getTimeDateFunctions)

  def typeInfo: Sql[ResultSet] =
    Try(ResultSet(x.getTypeInfo))

  def udts(catalog: Option[String], schemaPattern: Option[String], typeNamePattern: String, types: Option[Set[UserDefinedType]]): Sql[ResultSet] =
    Try(ResultSet(x.getUDTs(catalog.orNull, schemaPattern.orNull, typeNamePattern, types map (_ map (_.int) toArray) orNull)))

  def url: XSql[String] =
    XTry(x.getURL)

  def userName: Sql[String] =
    Try(x.getUserName)

  def versionColumns(catalog: Option[String], schema: Option[String], table: String): Sql[ResultSet] =
    Try(ResultSet(x.getVersionColumns(catalog.orNull, schema.orNull, table)))

  def insertsAreDetected(t: ResultSetType): Sql[Boolean] =
    Try(x.insertsAreDetected(t.int))

  def isCatalogAtStart: Sql[Boolean] =
    Try(x.isCatalogAtStart)

  def isReadOnly: Sql[Boolean] =
    Try(x.isReadOnly)

  def locatorsUpdateCopy: Sql[Boolean] =
    Try(x.locatorsUpdateCopy)

  def nullPlusNonNullIsNull: Sql[Boolean] =
    Try(x.nullPlusNonNullIsNull)

  def nullsAreSortedAtEnd: Sql[Boolean] =
    Try(x.nullsAreSortedAtEnd)

  def nullsAreSortedAtStart: Sql[Boolean] =
    Try(x.nullsAreSortedAtStart)

  def nullsAreSortedHigh: Sql[Boolean] =
    Try(x.nullsAreSortedHigh)

  def nullsAreSortedLow: Sql[Boolean] =
    Try(x.nullsAreSortedLow)

  def othersDeletesAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.othersDeletesAreVisible(t.int))

  def othersInsertsAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.othersInsertsAreVisible(t.int))

  def othersUpdatesAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.othersUpdatesAreVisible(t.int))

  def ownDeletesAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.ownDeletesAreVisible(t.int))

  def ownInsertsAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.ownInsertsAreVisible(t.int))

  def ownUpdatesAreVisible(t: ResultSetType): Sql[Boolean] =
    Try(x.ownUpdatesAreVisible(t.int))

  def storesLowerCaseIdentifiers: Sql[Boolean] =
    Try(x.storesLowerCaseIdentifiers)

  def storesLowerCaseQuotedIdentifiers: Sql[Boolean] =
    Try(x.storesLowerCaseQuotedIdentifiers)

  def storesMixedCaseIdentifiers: Sql[Boolean] =
    Try(x.storesMixedCaseIdentifiers)

  def storesMixedCaseQuotedIdentifiers: Sql[Boolean] =
    Try(x.storesMixedCaseQuotedIdentifiers)

  def storesUpperCaseIdentifiers: Sql[Boolean] =
    Try(x.storesUpperCaseIdentifiers)

  def storesUpperCaseQuotedIdentifiers: Sql[Boolean] =
    Try(x.storesUpperCaseQuotedIdentifiers)

  def supportsAlterTableWithAddColumn: Sql[Boolean] =
    Try(x.supportsAlterTableWithAddColumn)

  def supportsAlterTableWithDropColumn: Sql[Boolean] =
    Try(x.supportsAlterTableWithDropColumn)

  def supportsANSI92EntryLevelSQL: Sql[Boolean] =
    Try(x.supportsANSI92EntryLevelSQL)

  def supportsANSI92FullSQL: Sql[Boolean] =
    Try(x.supportsANSI92FullSQL)

  def supportsANSI92IntermediateSQL: Sql[Boolean] =
    Try(x.supportsANSI92IntermediateSQL)

  def supportsBatchUpdates: Sql[Boolean] =
    Try(x.supportsBatchUpdates)

  def supportsCatalogsInDataManipulation: Sql[Boolean] =
    Try(x.supportsCatalogsInDataManipulation)

  def supportsCatalogsInIndexDefinitions: Sql[Boolean] =
    Try(x.supportsCatalogsInIndexDefinitions)

  def supportsCatalogsInPrivilegeDefinitions: Sql[Boolean] =
    Try(x.supportsCatalogsInPrivilegeDefinitions)

  def supportsCatalogsInProcedureCalls: Sql[Boolean] =
    Try(x.supportsCatalogsInProcedureCalls)

  def supportsCatalogsInTableDefinitions: Sql[Boolean] =
    Try(x.supportsCatalogsInTableDefinitions)

  def supportsColumnAliasing: Sql[Boolean] =
    Try(x.supportsColumnAliasing)

  def supportsConvert(t: Option[(SqlType, SqlType)]): Sql[Boolean] =
    Try(t match {
      case None => x.supportsConvert
      case Some((from, to)) => x.supportsConvert(from.int, to.int)
    })

  def supportsCoreSQLGrammar: Sql[Boolean] =
    Try(x.supportsCoreSQLGrammar)

  def supportsCorrelatedSubqueries: Sql[Boolean] =
    Try(x.supportsCorrelatedSubqueries)

  def supportsDataDefinitionAndDataManipulationTransactions: Sql[Boolean] =
    Try(x.supportsDataDefinitionAndDataManipulationTransactions)

  def supportsDataManipulationTransactionsOnly: Sql[Boolean] =
    Try(x.supportsDataManipulationTransactionsOnly)

  def supportsDifferentTableCorrelationNames: Sql[Boolean] =
    Try(x.supportsDifferentTableCorrelationNames)

  def supportsExpressionsInOrderBy: Sql[Boolean] =
    Try(x.supportsExpressionsInOrderBy)

  def supportsExtendedSQLGrammar: Sql[Boolean] =
    Try(x.supportsExtendedSQLGrammar)

  def supportsFullOuterJoins: Sql[Boolean] =
    Try(x.supportsFullOuterJoins)

  def supportsGetGeneratedKeys: Sql[Boolean] =
    Try(x.supportsGetGeneratedKeys)

  def supportsGroupBy: Sql[Boolean] =
    Try(x.supportsGroupBy)

  def supportsGroupByBeyondSelect: Sql[Boolean] =
    Try(x.supportsGroupByBeyondSelect)

  def supportsGroupByUnrelated: Sql[Boolean] =
    Try(x.supportsGroupByUnrelated)

  def supportsIntegrityEnhancementFacility: Sql[Boolean] =
    Try(x.supportsIntegrityEnhancementFacility)

  def supportsLikeEscapeClause: Sql[Boolean] =
    Try(x.supportsLikeEscapeClause)

  def supportsLimitedOuterJoins: Sql[Boolean] =
    Try(x.supportsLimitedOuterJoins)

  def supportsMinimumSQLGrammar: Sql[Boolean] =
    Try(x.supportsMinimumSQLGrammar)

  def supportsMixedCaseIdentifiers: Sql[Boolean] =
    Try(x.supportsMixedCaseIdentifiers)

  def supportsMixedCaseQuotedIdentifiers: Sql[Boolean] =
    Try(x.supportsMixedCaseQuotedIdentifiers)

  def supportsMultipleOpenResults: Sql[Boolean] =
    Try(x.supportsMultipleOpenResults)

  def supportsMultipleResultSets: Sql[Boolean] =
    Try(x.supportsMultipleResultSets)

  def supportsMultipleTransactions: Sql[Boolean] =
    Try(x.supportsMultipleTransactions)

  def supportsNamedParameters: Sql[Boolean] =
    Try(x.supportsNamedParameters)

  def supportsNonNullableColumns: Sql[Boolean] =
    Try(x.supportsNonNullableColumns)

  def supportsOpenCursorsAcrossCommit: Sql[Boolean] =
    Try(x.supportsOpenCursorsAcrossCommit)

  def supportsOpenCursorsAcrossRollback: Sql[Boolean] =
    Try(x.supportsOpenCursorsAcrossRollback)

  def supportsOpenStatementsAcrossCommit: Sql[Boolean] =
    Try(x.supportsOpenStatementsAcrossCommit)

  def supportsOpenStatementsAcrossRollback: Sql[Boolean] =
    Try(x.supportsOpenStatementsAcrossRollback)

  def supportsOrderByUnrelated: Sql[Boolean] =
    Try(x.supportsOrderByUnrelated)

  def supportsOuterJoins: Sql[Boolean] =
    Try(x.supportsOuterJoins)

  def supportsPositionedDelete: Sql[Boolean] =
    Try(x.supportsPositionedDelete)

  def supportsPositionedUpdate: Sql[Boolean] =
    Try(x.supportsPositionedUpdate)

  def supportsResultSetConcurrency(t: ResultSetType, concurrency: ResultSetConcurrency): Sql[Boolean] =
    Try(x.supportsResultSetConcurrency(t.int, concurrency.int))

  def supportsResultSetHoldability(holdability: ResultSetHoldability): Sql[Boolean] =
    Try(x.supportsResultSetHoldability(holdability.int))

  def supportsResultSetType(t: ResultSetType): Sql[Boolean] =
    Try(x.supportsResultSetType(t.int))

  def supportsSavepoints: Sql[Boolean] =
    Try(x.supportsSavepoints)

  def supportsSchemasInDataManipulation: Sql[Boolean] =
    Try(x.supportsSchemasInDataManipulation)

  def supportsSchemasInIndexDefinitions: Sql[Boolean] =
    Try(x.supportsSchemasInIndexDefinitions)

  def supportsSchemasInPrivilegeDefinitions: Sql[Boolean] =
    Try(x.supportsSchemasInPrivilegeDefinitions)

  def supportsSchemasInProcedureCalls: Sql[Boolean] =
    Try(x.supportsSchemasInProcedureCalls)

  def supportsSchemasInTableDefinitions: Sql[Boolean] =
    Try(x.supportsSchemasInTableDefinitions)

  def supportsSelectForUpdate: Sql[Boolean] =
    Try(x.supportsSelectForUpdate)

  def supportsStatementPooling: Sql[Boolean] =
    Try(x.supportsStatementPooling)

  def supportsStoredProcedures: Sql[Boolean] =
    Try(x.supportsStoredProcedures)

  def supportsSubqueriesInComparisons: Sql[Boolean] =
    Try(x.supportsSubqueriesInComparisons)

  def supportsSubqueriesInExists: Sql[Boolean] =
    Try(x.supportsSubqueriesInExists)

  def supportsSubqueriesInIns: Sql[Boolean] =
    Try(x.supportsSubqueriesInIns)

  def supportsSubqueriesInQuantifieds: Sql[Boolean] =
    Try(x.supportsSubqueriesInQuantifieds)

  def supportsTableCorrelationNames: Sql[Boolean] =
    Try(x.supportsTableCorrelationNames)

  def supportsTransactionIsolationLevel(level: TransactionIsolation): Sql[Boolean] =
    Try(x.supportsTransactionIsolationLevel(level.int))

  def supportsTransactions: Sql[Boolean] =
    Try(x.supportsTransactions)

  def supportsUnion: Sql[Boolean] =
    Try(x.supportsUnion)

  def supportsUnionAll: Sql[Boolean] =
    Try(x.supportsUnionAll)

  def updatesAreDetected(t: ResultSetType): Sql[Boolean] =
    Try(x.updatesAreDetected(t.int))

  def usesLocalFilePerTable: Sql[Boolean] =
    Try(x.usesLocalFilePerTable)

  def usesLocalFiles: Sql[Boolean] =
    Try(x.usesLocalFiles)

}

object DatabaseMetaData {
  def apply(xx: D): DatabaseMetaData =
    new DatabaseMetaData {
      val x = xx
    }

  sealed trait BestRow {
    def int: Int =
      this match {
        case BestRow.Unknown => D.bestRowUnknown
        case BestRow.NotPseudo => D.bestRowNotPseudo
        case BestRow.Pseudo => D.bestRowPseudo
      }
  }

  object BestRow {
    case object Unknown extends BestRow
    case object NotPseudo extends BestRow
    case object Pseudo extends BestRow
  }

}
