package com.ephox.vault

import org.scalatest.FunSuite
import com.ephox.vault._
import scalaz._
import Scalaz._

class MergerTest extends FunSuite {
  case class Billy(key: Key, b: String)
  case class Jimmy(key: Key, j: String)
  case class Bob(key: Key, x: String, bs: List[Billy], js: List[Jimmy]) {
    override def toString =
      "Bob[\n" +
      "  key = " + key + "\n" +
      "  x = " + x + "\n" +
      "  bs = {\n       " + bs.mkString(",\n       ") + "\n       }\n" +
      "  js = {\n       " + js.mkString(",\n       ") + "\n       }\n" +
      "]"
  }

  implicit def KeyedBilly: Keyed[Billy] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def KeyedBob: Keyed[Bob] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def KeyedJimmy: Keyed[Jimmy] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def MergerBilly: Merger[Billy] =
    merge0

  implicit def MergerJimmy: Merger[Jimmy] =
    merge0

  implicit def MergerBob: Merger[Bob] =
    merge2n[Bob, Billy, Jimmy](_.bs, _.js, (bob, bs, js) => bob.copy(bs = bs, js = js))

  val billy1 = Billy(key(1), "billy1")
  val billy2 = Billy(key(2), "billy2")

  val billy3 = Billy(key(3), "billy3")
  val billy4 = Billy(key(4), "billy4")

  val jimmy0 = Jimmy(key(200), "jimmy0")
  val jimmy1 = Jimmy(key(201), "jimmy1")
  val jimmy2 = Jimmy(key(202), "jimmy2")

  val jimmy3 = Jimmy(key(203), "jimmy3")
  val jimmy4 = Jimmy(key(204), "jimmy4")

  val bobs = List(
    Bob(key(100), "bob1", List(billy1), List(jimmy0)),
    Bob(key(100), "bob1", List(billy2), List(jimmy0)),
    Bob(key(100), "bob1", List(billy1), List(jimmy1)),
    Bob(key(100), "bob1", List(billy2), List(jimmy1)),
    Bob(key(100), "bob1", List(billy1), List(jimmy2)),
    Bob(key(100), "bob1", List(billy2), List(jimmy2)),
    Bob(key(101), "bob2", List(billy3), List(jimmy3)),
    Bob(key(101), "bob2", List(billy4), List(jimmy3)),
    Bob(key(101), "bob2", List(billy3), List(jimmy4)),
    Bob(key(101), "bob2", List(billy4), List(jimmy4))
  )

  test("merge") {
    val result = ListEnumerator(bobs, combineAll[Bob]).run
    println(result)
  }

  implicit val ListEnumerator = new Enumerator[List] {
    def apply[E, A](e: List[E], i: IterV[E, A]): IterV[E, A] = e match {
      case List() => i
      case x :: xs => i.fold(done = (_, _) => i, cont = k => apply(xs, k(IterV.El(x))))
    }
  }
}
