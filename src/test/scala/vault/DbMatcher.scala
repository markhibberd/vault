package vault
import org.specs2._, matcher._, execute._
import scalaz.{Success => _, Failure => _, _}, effect.IO

object DbMatcher extends ThrownExpectations {
  def beOk[A]: Matcher[Db[A]] =
    beOkLike(_ => Success())

  def beOkValue[A](expected: A): Matcher[Db[A]] =
    beOkLike((actual: A) => new BeEqualTo(expected).apply(createExpectable(actual)).toResult)

  def beOkLike[A](check: A => Result): Matcher[Db[A]] = new Matcher[Db[A]] {
    def apply[S <: Db[A]](attempt: Expectable[S]) = {
      val c = Connector.hsqlmem(java.util.UUID.randomUUID.toString, "sa", "").create()
      try {
        val r = attempt.value.run(c).run.run.attemptRun match {
          case -\/(error)     => error.printStackTrace; Failure(s"Db failed with <${error}>")
          case \/-((log, a))  => a.toEither match {
            case -\/(DbException(error))  => error.printStackTrace; Failure(s"DbResult failed with <${error}>")
            case -\/(error)               => Failure(s"DbResult failed with <${error}>")
            case \/-(aa)                  => check(aa)
          }
        }
        result(r.isSuccess, r.message, r.message, attempt)
      } finally {
        c.prepareStatement("SHUTDOWN").execute
      }
    }
  }
}
