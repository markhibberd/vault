package vault

import scalaz._, Scalaz._
import java.sql.Connection

case class Db[+A](run: ReaderWriterDbValue[A])

object Db {
  def apply[A](run: Connection => WriterDbValue[A]): Db[A] =
    Db(Kleisli(run))
}
