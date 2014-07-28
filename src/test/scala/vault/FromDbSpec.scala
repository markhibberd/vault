package vault

import org.specs2._, matcher._, specification._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class FromDbSpec extends Specification with ScalaCheck { def is = s2"""

FromDb Spec
-----------

  Witness basics                        ${ok}
  Witness tuples                        ${ok}
  Witness auto                          ${ok}
  Witness derived                       ${ok}

"""

  object primitives {
    FromDb.of[String]
    FromDb.of[Int]
    FromDb.of[Boolean]
    FromDb.of[Long]
    FromDb.of[Double]
    FromDb.of[Float]
    FromDb.of[Short]
    FromDb.of[Byte]
    FromDb.of[BigDecimal]
    FromDb.of[java.sql.Date]
    FromDb.of[java.sql.Time]
    FromDb.of[java.sql.Timestamp]
    FromDb.of[Option[Int]]
    FromDb.of[Option[String]]
    FromDb.of[Keyed[Int]]
    FromDb.of[Keyed[String]]
  }

  object tuples {
    FromDb.of[(String, Int)]
    FromDb.of[(String, Int, Boolean)]
    FromDb.of[(String, Int, Boolean, Long)]
    FromDb.of[(String, Int, Boolean, Long, Double)]
    FromDb.of[(String, Int, Boolean, Long, Double, Float, java.sql.Date, Short, Byte, BigDecimal, java.sql.Time, java.sql.Timestamp, String, Int, Boolean, Long, Double, Float, java.sql.Date, Short, Byte, BigDecimal)]
  }

  object auto {
    import shapeless._
    import FromDb.auto._

    case class Person(name: String, age: Int)

//    FromDb.of[Person]

    FromDb.of[Person](AutoFromDb[Person])

  }

  object derived {
    case class Person(name: String, age: Int)

    implicit def PersonFromDb: FromDb[Person] =
      FromDb.derive[Person]

    FromDb.of[Person]
  }
}
