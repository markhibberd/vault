package vault


import com.clarifi.machines._
import java.sql.{Connection, SQLException}
import scalaz._, Scalaz._
import DbValue.{db, freedb}
import DbProcess._
import Db._

object Execute {
  def get_[A: FromDb](sql: String): Db[Option[A]] =
    get[Unit, A](sql, ())

  def list_[A: FromDb](sql: String): Db[List[A]] =
    list[Unit, A](sql, ())

  def get[A: ToDb, B: FromDb](sql: String, a: A): Db[Option[B]] = Db(conn => {
    val r = query(conn, sql, a, head[B]).foldLeftM(none[B])((_, b) => b)
    WriterT.put(r)(Vector[String]())
  })

  def list[A: ToDb, B: FromDb](sql: String, a: A): Db[List[B]] = Db(conn => {
    val f = freedomlist(conn, sql, a)

    var buffer = scala.collection.mutable.ListBuffer[B]()
    @scala.annotation.tailrec
    def go(next: FreeDb[Vector[B]]): DbFailure \/ scala.collection.mutable.ListBuffer[B] = next.resume match {
      case -\/(x) => x.toEither match {
        case -\/(xx) => xx.left
        case \/-(xx) => go(xx)
      }
      case \/-(x) => { buffer ++= x; buffer.right }
    }

    val r = DbValue(go(f).map(_.toList))
    WriterT.put(r)(Vector[String]())
  })


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

  def freedomlist[A: ToDb, B: FromDb](conn: Connection, sql: String, a: A): FreeDb[Vector[B]] =
    freedom(conn, sql, a, Process.wrapping[B]).execute

  def freedom[A: ToDb, B: FromDb, C](conn: Connection, sql: String, a: A, m: Process[B, C]): Procedure[FreeDb, C] =
    new Procedure[FreeDb, C] {
      type K = B => Any
      val machine = m
      def withDriver[R](k: Driver[FreeDb, K] => FreeDb[R]): FreeDb[R] = for {
        s <- freedb { conn.prepareStatement(sql) }
        _ <- ToDb.set[A].execute(Sql.jdbc(s), a).free
        rs <- freedb { s.executeQuery }
        r <- k(new Driver [FreeDb, K] {
          val M = Monad[FreeDb]
          def apply(k: K) = for {
            row_ <- freedb { if (rs.next) Row.jdbc(rs).some else none }
            b <- (row_ match {
              case Some(row) =>
                FromDb.get[B].perform(row).map(_.some)
              case None =>
                none.pure[DbValue]
            }).free
          } yield b
        })
      } yield r
    }


}
