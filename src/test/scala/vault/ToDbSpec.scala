package vault

import org.specs2._, matcher._, specification._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class ToDbSpec extends Specification with ScalaCheck { def is = s2"""

ToDb Spec
---------

  Witness basics                        ${ok}
  Witness tuples                        ${ok}
  Witness auto                          ${ok}
  Witness derived                       ${ok}

"""

  object primitives {
    ToDb.of[String]
    ToDb.of[Int]
    ToDb.of[Boolean]
    ToDb.of[Long]
    ToDb.of[Double]
    ToDb.of[Float]
    ToDb.of[Short]
    ToDb.of[Byte]
    ToDb.of[BigDecimal]
    ToDb.of[java.sql.Date]
    ToDb.of[java.sql.Time]
    ToDb.of[java.sql.Timestamp]
    ToDb.of[Option[Int]]
    ToDb.of[Option[String]]
    ToDb.of[Keyed[Int]]
    ToDb.of[Keyed[String]]
  }

  object tuples {
    ToDb.of[(String, Int)]
    ToDb.of[(String, Int, Boolean)]
    ToDb.of[(String, Int, Boolean, Long)]
    ToDb.of[(String, Int, Boolean, Long, Double)]
    ToDb.of[(String, Int, Boolean, Long, Double, Float, java.sql.Date, Short, Byte, BigDecimal, java.sql.Time, java.sql.Timestamp, String, Int, Boolean, Long, Double, Float, java.sql.Date, Short, Byte, BigDecimal)]
  }

  object auto {
    import ToDb.auto._

    case class Person(name: String, age: Int)

    ToDb.of[Person]
  }

  object derived {
    case class Person(name: String, age: Int)

    implicit def PersonToDb: ToDb[Person] =
      ToDb.derive[Person]

    ToDb.of[Person]
  }
}
