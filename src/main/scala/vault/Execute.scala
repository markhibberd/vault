package vault


import com.clarifi.machines._
import java.sql.{Connection, SQLException}
import scalaz._, Scalaz._
import DbValue.db

object Execute {
  def one[A]: Process[A, Option[A]] =
    (Plan.await[A] flatMap(a => Plan.emit(a.some))).orElse(Plan.emit(none[A]) >> Stop).compile

  def get[A: ToDb, B: FromDb](conn: Connection, sql: String, a: A): DbValue[Option[B]] =
    query(conn, sql, a, one[B]).foldLeftM(none[B])((_, b) => b)

  def list[A: ToDb, B: FromDb](conn: Connection, sql: String, a: A): DbValue[Vector[B]] =
    query(conn, sql, a, Process.wrapping[B]).execute

  def query[A: ToDb, B: FromDb, C](conn: Connection, sql: String, a: A, m: Process[B, C]): Procedure[DbValue, C] =
    new Procedure[DbValue, C] {
      type K = B => Any
      val machine = m
      def withDriver[R](k: Driver[DbValue, K] => DbValue[R]): DbValue[R] = for {
        s <- db { conn.prepareStatement(sql) }
        _ <- ToDb.set[A].execute(Sql.jdbc(s), a)
        rs <- db { s.executeQuery }
        r <- k(new Driver [DbValue, K] {
          val M = Monad[DbValue]
          def apply(k: K) = for {
            row_ <- db { if (rs.next) Row.jdbc(rs).some else none }
            b <- row_ match {
              case Some(row) =>
                FromDb.get[B].perform(row).map(_.some)
              case None =>
                none.pure[DbValue]
            }
          } yield b
        })
      } yield r
    }
}
