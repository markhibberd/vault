import scalaz._, Scalaz._, concurrent._
import java.sql.Connection

package object vault {
  type Context_[+A] = DbValueT[Task, A]
  type Context[+A] = ReaderT[Context_, DbRead, A]
}
