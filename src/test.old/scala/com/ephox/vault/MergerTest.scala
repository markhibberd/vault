package com.ephox.vault

import org.scalatest.FunSuite
import com.ephox.vault._, Vault._
import scalaz._, Scalaz._, iteratee._

class MergerTest extends FunSuite {
  case class Billy(key: Key, b: String) {
    override def toString = "Billy[" + key.fold("", _ + "") + "]"
  }

  case class Jimmy(key: Key, j: String) {
    override def toString = "Jimmy[" + key.fold("", _ + "") + "]"
  }
  case class Bob(key: Key, x: String, bs: List[Billy], js: List[Jimmy]) {
    override def toString =
      "Bob[" + key.fold("", _ + "") + ", [" + bs.mkString(",") + "], [" + js.mkString(",") + "]]"
  }
  case class Master(key: Key, bobs: List[Bob]) {
    override def toString =
      "Master[" + key.fold("", _ + "") + ", [" + bobs.mkString(",") + "]]"
  }

  implicit def KeyedBilly: Keyed[Billy] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def KeyedBob: Keyed[Bob] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def KeyedJimmy: Keyed[Jimmy] =
    keyed(_.key, (b, k) => b.copy(key = k))

  implicit def KeyedMaster: Keyed[Master] =
    keyed(_.key, (m, k) => m.copy(key = k))

  implicit def MergerBilly: Merger[Billy] =
    merge0

  implicit def MergerJimmy: Merger[Jimmy] =
    merge0

  implicit def MergerBob: Merger[Bob] =
    merge2n[Bob, Billy, Jimmy](_.bs, _.js, (bob, bs, js) => bob.copy(bs = bs, js = js))

  implicit def MergerMaster: Merger[Master] =
    merge1n[Master, Bob](_.bobs, (m, bobs) => m.copy(bobs = bobs))

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

  val masters = List(
    Master(key(900), Bob(key(100), "bob1", List(billy1), List(jimmy0)) :: Nil),
    Master(key(900), Bob(key(100), "bob1", List(billy2), List(jimmy0)) :: Nil),
    Master(key(900), Bob(key(100), "bob1", List(billy1), List(jimmy1)) :: Nil),
    Master(key(900), Bob(key(100), "bob1", List(billy2), List(jimmy1)) :: Nil),
    Master(key(900), Bob(key(100), "bob1", List(billy1), List(jimmy2)) :: Nil),
    Master(key(900), Bob(key(100), "bob1", List(billy2), List(jimmy2)) :: Nil),
    Master(key(900), Bob(key(101), "bob2", List(billy3), List(jimmy3)) :: Nil),
    Master(key(900), Bob(key(101), "bob2", List(billy4), List(jimmy3)) :: Nil),
    Master(key(900), Bob(key(101), "bob2", List(billy3), List(jimmy4)) :: Nil),
    Master(key(900), Bob(key(101), "bob2", List(billy4), List(jimmy4)) :: Nil)
  )


  test("merge 2n") {
    val e = EnumeratorT.enumList[Bob, Id](bobs)
    val result = combineAll[Bob] >>== (e.apply[List[Bob]](_))
    val z = List(
      Bob(key(100), "bob1", List(billy1, billy2), List(jimmy0, jimmy1, jimmy2)),
      Bob(key(101), "bob2", List(billy3, billy4), List(jimmy3, jimmy4))
    )
    expect(result.run)(z)
  }

  test("merge nested 1n/2n") {
    val e = EnumeratorT.enumList[Master, Id](masters)
    val result = combine[Master] >>== (e.apply[Option[Master]](_))
    val z = Some(
      Master(key(900)
        , Bob(key(100), "bob1", List(billy1, billy2), List(jimmy0, jimmy1, jimmy2))
       :: Bob(key(101), "bob2", List(billy3, billy4), List(jimmy3, jimmy4))
       :: Nil
      )
    )

    expect(result.run)(z)
  }
}
