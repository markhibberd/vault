package vault

import org.scalacheck._, Arbitrary._


case class Person(name: String, age: Int)

object Person {
  def table =
    "CREATE TABLE person (id IDENTITY, name VARCHAR(255), age INTEGER)"

  implicit def PersonArbitrary: Arbitrary[Person] =
    Arbitrary(for { n <- Gen.identifier; a <- Gen.choose(0, 120) } yield Person(n, a))

  implicit def PersonToDb: ToDb[Person] =
    ToDb.derive[Person]

  implicit def PersonFromDb: FromDb[Person] =
    FromDb.derive[Person]
}
