package vault

import java.sql.Connection

import org.specs2._, org.specs2.matcher._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._, concurrent._
import vault.DbMatcher._

class ExecuteSpec extends Specification with ScalaCheck { def is = s2"""

Execute Spec
------------

  execute                     $execute
  update                      $update
  list                        $list
  get                         $get
  process                     $process

"""

  implicit val params =
    Parameters(maxSize = 10)

  def execute =
    Execute.execute_(Person.table) must beOk

  def setup(ps: List[Person]): Db[Unit] =
    Execute.execute_(Person.table) >>
      ps.traverse(Execute.execute("INSERT INTO person (name, age) VALUES (?, ?)", _)).void

  def update = prop((p: Person) =>
    (setup(Nil) >> Execute.update[Person]("INSERT INTO person (name, age) VALUES (?, ?)", p)) must beOkValue(1))

  def list = prop((ps: List[Person]) =>
    (setup(ps) >> Execute.list_[Person]("SELECT name, age FROM person")) must beOkValue(ps))

  def get = prop((ps: List[Person]) => !ps.isEmpty ==> {
    (setup(ps) >> Execute.get[String, Person]("SELECT name, age FROM person WHERE name = ?", ps.head.name)) must beOkValue(ps.headOption) })

  def process = prop((ps: List[Person]) =>
    (setup(ps) >> Execute.process[Unit, Person]("SELECT name, age FROM person", ()).runLog.map(_.toList)) must beOkValue(ps))
}
