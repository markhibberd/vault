package vault

import com.clarifi.machines._
import scalaz._, Scalaz._

// FIX Do these belong in the machines library?
object DbProcess {
  def head[A]: Process[A, Option[A]] =
    (Plan.await[A] flatMap(a => Plan.emit(a.some))).orElse(
      Plan.emit(none[A]) >> Stop).compile
}
