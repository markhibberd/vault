package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait SqlError {
  def exception: Option[java.sql.SQLException] =
    this match {
      case SqlException(e) => Some(e)
      case SqlWarning(e) => Some(e)
      case DataTruncation(e) => Some(e)
      case BatchUpdateException(e) => Some(e)
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def reason: Option[Cord] =
    exception flatMap (x => Option(x.getMessage))

  def state: Option[Cord] =
    exception flatMap (x => Option(x.getSQLState))

  def code: Option[Int] =
    exception flatMap (x => Option(x.getErrorCode))

  def isSqlException: Boolean =
    this match {
      case SqlException(_) => true
      case SqlWarning(_) => false
      case DataTruncation(_) => false
      case BatchUpdateException(_) => false
      case IncompatibilityUnexpectedInt(_, _) => false
    }

  def isWarning: Boolean =
    this match {
      case SqlException(_) => false
      case SqlWarning(_) => true
      case DataTruncation(_) => false
      case BatchUpdateException(_) => false
      case IncompatibilityUnexpectedInt(_, _) => false
    }

  def isDataTruncation: Boolean =
    this match {
      case SqlException(_) => false
      case SqlWarning(_) => false
      case DataTruncation(_) => true
      case BatchUpdateException(_) => false
      case IncompatibilityUnexpectedInt(_, _) => false
    }

  def isBatchUpdateException: Boolean =
    this match {
      case SqlException(_) => false
      case SqlWarning(_) => false
      case DataTruncation(_) => false
      case BatchUpdateException(_) => true
      case IncompatibilityUnexpectedInt(_, _) => false
    }

  def isIncompatibilityUnexpectedInt: Boolean =
    this match {
      case SqlException(_) => false
      case SqlWarning(_) => false
      case DataTruncation(_) => false
      case BatchUpdateException(_) => false
      case IncompatibilityUnexpectedInt(_, _) => true
    }

  def dataSize: Option[Int] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(e) => Some(e.getDataSize)
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def index: Option[Int] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(e) => Some(e.getIndex)
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def transferSize: Option[Int] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(e) => Some(e.getTransferSize)
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def parameter: Option[Boolean] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(e) => Some(e.getParameter)
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def read: Option[Boolean] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(e) => Some(e.getRead)
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def updateCounts: Option[List[Int]] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(_) => None
      case BatchUpdateException(e) => Some(e.getUpdateCounts.toList)
      case IncompatibilityUnexpectedInt(_, _) => None
    }

  def incompatibilityUnexpectedInt: Option[Int] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(_) => None
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(n, _) => Some(n)
    }

  def incompatibilityUnexpectedIntName: Option[String] =
    this match {
      case SqlException(_) => None
      case SqlWarning(_) => None
      case DataTruncation(_) => None
      case BatchUpdateException(_) => None
      case IncompatibilityUnexpectedInt(_, s) => Some(s)
    }

}
private case class SqlException(ex: java.sql.SQLException) extends SqlError
private case class SqlWarning(ex: java.sql.SQLWarning) extends SqlError
private case class DataTruncation(ex: java.sql.DataTruncation) extends SqlError
private case class BatchUpdateException(ex: java.sql.BatchUpdateException) extends SqlError
private case class IncompatibilityUnexpectedInt(n: Int, s: String) extends SqlError

object SqlError extends SqlErrorFunctions

trait SqlErrorFunctions {
  private[sql] def sqlException(e: java.sql.SQLException): SqlError =
    SqlException(e)

  private[sql] def sqlWarning(e: java.sql.SQLWarning): SqlError =
    SqlWarning(e)

  private[sql] def dataTruncation(e: java.sql.DataTruncation): SqlError =
    DataTruncation(e)

  private[sql] def batchUpdateException(e: java.sql.BatchUpdateException): SqlError =
    BatchUpdateException(e)

  private[sql] def incompatibilityUnexpectedInt(n: Int, s: String): SqlError =
    IncompatibilityUnexpectedInt(n, s)

}
