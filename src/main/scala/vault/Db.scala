package vault

import scalaz._, Scalaz._
import java.sql.Connection

case class Db[+A](runDb: ReaderWriterDbValue[A]) {
  def apply(connection: Connection): DbValue[(Log, A)] =
    runDb(connection).run
}

object Db {
  def apply[A](run: Connection => WriterDbValue[A]): Db[A] =
    Db(Kleisli(run))
}
