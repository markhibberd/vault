package com.ephox.vault

import java.sql._

final case class Transaction[A](connect: Connection => Either[SQLException, A]) {
  def map[B](f: A => B): Transaction[B] =
    Transaction(c => connect(c).fold(l => Left(l), r => Right(f(r))))

  def flatMap[B](f: A => Transaction[B]): Transaction[B] =
    Transaction(c => connect(c).fold(l => Left(l), r => f(r).connect(c)))
}

object Transaction {
  import StatementBinder._

  def evaluate[A](connector: SQLConnect, tx: Transaction[A]): Either[SQLException, A] = {
    val connection = connector.nu
    try {
      tx.connect(connection)
    } catch {
      case e => connection.rollback; throw e
    } finally {
      connection.commit
    }
  }

  def runQuery[A](tx: Connection)(q: Query, f: ResultSet => A): Either[SQLException, A] =
    q.fold((sql, params) =>
        statement(tx, sql, params) { stmt =>
          val rs = stmt.executeQuery()
          f(rs)
        })

  def executeDdl(tx: Connection)(s: DdlStatement): Either[SQLException, Unit] =
    s.fold(ddls =>
      ddls.foldLeft[Either[SQLException, Unit]](Right(())) {
        (acc, sql) =>
          acc.right.flatMap(_ =>
            statement(tx, sql, List()) { stmt =>
                stmt.execute()
                ()
             })
      })

  def executeDmlWithKeys[A](tx: Connection)(s: DmlStatement, f:(Int, ResultSet) => A): Either[SQLException, A] =
      s.fold((sql, params) =>
        statement(tx, sql, params, Statement.RETURN_GENERATED_KEYS) { stmt =>
          val count = stmt.executeUpdate()
          val keys = stmt.getGeneratedKeys
          f(count, keys)
        })

  def executeDml(tx: Connection)(s: DmlStatement): Either[SQLException, Int] =
      s.fold((sql, params) =>
        statement(tx, sql, params) { stmt =>
          val count = stmt.executeUpdate()
          count
        })

   def executeDmls(tx: Connection)(dmls: List[DmlStatement]): Either[SQLException, Int] =
      dmls.foldLeft[Either[SQLException, Int]](Right(0)) {
        case (acc, stmt) =>
          for {
            c1 <- acc.right
            c2 <- executeDml(tx)(stmt).right
          } yield c1 + c2
      }
}
