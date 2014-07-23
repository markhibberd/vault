import scalaz._, Scalaz._, concurrent._
import java.sql.Connection

package object vault {
  type Context__[+A] = WriterT[Task, DbHistory, A]
  type Context_[+A] = DbValueT[Context__, A]
  type Context[+A] = ReaderT[Context_, Connection, A]
}
