import scalaz._, Scalaz._, Free._
import java.sql.Connection

package object vault {
  type Log = Vector[String]
  type FreeDb[+A] = Free[DbValue, A]
  type WriterDbValue[+A] = WriterT[DbValue, Log, A]
  type ReaderWriterDbValue[+A] = ReaderT[WriterDbValue, Connection, A]
}
