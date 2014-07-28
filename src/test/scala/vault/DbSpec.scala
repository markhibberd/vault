package vault

import java.sql.Connection

import org.specs2._, matcher._, specification._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._, concurrent._

class DbSpec extends Specification with ScalaCheck { def is = s2"""

Db Spec
-------

  Catchable works -
    ${attempt}
    ${fail}

"""
  def withConnection[A](f: Connection => A): A =
    f(Connector.hsqltest.create())

  def attempt = prop((t: Throwable) =>
    withConnection(connection =>
      Catchable[Db].attempt(Db.liftTask(Task.delay(throw t))).run(connection).run.run.run must_== DbHistory.empty -> DbValue.ok(t.left)))

  def fail = prop((t: Throwable) =>
    withConnection(connection =>
      Catchable[Db].fail(t).run(connection).run.run.attemptRun must_== t.left))
}
