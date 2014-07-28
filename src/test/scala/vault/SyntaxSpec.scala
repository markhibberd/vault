package vault

import java.sql.Connection

import org.specs2._, org.specs2.matcher._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._, concurrent._
import vault.DbMatcher._

class SyntaxSpec extends Specification with ScalaCheck { def is = s2"""

Syntax Spec
------------

  execute                     $execute
  update                      $update
  list                        $list
  get                         $get
  process                     $process
  bind                        $bind
  expand                      $expand

"""

  import vault.Syntax._

  implicit val params =
    Parameters(maxSize = 10)

  def setup(ps: List[Person]): Db[Unit] =
    Execute.execute_(Person.table) >>
      ps.traverse(Execute.execute("INSERT INTO person (name, age) VALUES (?, ?)", _)).void

  def execute = prop((p: Person)  =>
    setup(Nil) >> q"INSERT INTO person (name, age) VALUES ($p)".execute must beOk)

  def update = prop((p: Person) =>
    (setup(Nil) >> q"INSERT INTO person (name, age) VALUES ($p)".update) must beOkValue(1))

  def list = prop((ps: List[Person]) =>
    (setup(ps) >> q"SELECT name, age FROM person".list[Person]) must beOkValue(ps))

  def get = prop((ps: List[Person]) => !ps.isEmpty ==> {
    (setup(ps) >> q"SELECT name, age FROM person WHERE name = ${ps.head.name}".get[Person]) must beOkValue(ps.headOption) })

  def process = prop((ps: List[Person]) =>
    (setup(ps) >> q"SELECT name, age FROM person".process[Person].runLog.map(_.toList)) must beOkValue(ps))

  def bind = prop((p: Person) =>
    (setup(Nil) >>
     q"INSERT INTO person (name, age) VALUES (?, ?)".bind(p).execute >>
     q"SELECT name, age FROM person".list[Person]) must beOkValue(List(p)) )

  def expand = prop((p: Person) =>
    (setup(Nil) >>
     q"INSERT INTO person (name, age) VALUES (${p})".execute >>
     q"SELECT name, age FROM person".list[Person]) must beOkValue(List(p)) )

}
