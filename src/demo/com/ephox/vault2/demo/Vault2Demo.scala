package com.ephox.vault2.demo

import scalaz._
import Scalaz._
import com.ephox.vault2._
import java.sql.Connection

object Vault2Demo {
  case class Person(name: String, age: Int)

  object Person {
    implicit val ShowPerson: Show[Person] = showA
  }

  val data =
    List(
          "Bob" -> 45
        , "Bob" -> 54
        , "Mary" -> 78
        , "Fred" -> 99
        , "Jack" -> 9999
        , "Mark" -> 9999
        , "Mark" -> 9999
        ) map { case (name, age) => Person(name, age) }

  val PersonRowAccess =
    for {
      name <- stringIndex(2)
      age  <- intIndex(3)
    } yield Person(name, age)

  def setupData =
    for {
      a <- "DROP TABLE IF EXISTS PERSON".executeUpdate
      b <- "CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER)".executeUpdate
      p <- "INSERT INTO PERSON(name, age) VALUES (?,?)" prepareStatement
             (s => s.foreachStatement(data, (p: Person) => p match {
               case Person(name, age) => {
                 s.set(stringType(name), intType(age))
               }
             }))
    } yield a + b + p

  def groupBy[A](pred: (A, A) => Boolean): IterV[A, List[A]] = {
    IterV.peek >>= {
      case None => IterV.Done(Nil, IterV.Empty.apply)
      case Some(h) => takeWhile(pred(_, h))
    }
  }

  def optionIterV[A, B](b: B, f: A => IterV[A, B]): Option[A] => IterV[A, B] = {
    case None => IterV.Done(b, IterV.EOF.apply)
    case Some(a) => f(a)
  }

  def peekOptionIterV[A, B](b: B, f: A => IterV[A, B]): IterV[A, B] = IterV.peek >>= optionIterV(b, f)

  def drop1Then[E, A](i: => IterV[E, A]): IterV[E, A] =
    IterV.drop(1) >>=| i
  
  def takeWhile[A](pred: A => Boolean): IterV[A, List[A]] = {
    def peekStepOption(z: List[A]) = peekOptionIterV(z, step(z, _: A))

    def step(acc: List[A], a: A): IterV[A, List[A]] = {
      if (pred(a))
        drop1Then(peekStepOption(acc ::: List(a)))
      else
        IterV.Done(acc, IterV.EOF.apply)
    }
    peekStepOption(Nil)
  }

  def repeatList[E,A](iter: IterV[E,A]): IterV[E, List[A]] = {
	  def step(s: List[A]): Input[E] => IterV[E, List[A]] = {
	    case IterV.EOF() => IterV.Done(s, IterV.EOF.apply)
	    case IterV.Empty() => IterV.Cont(step(s))
	    case IterV.El(e) => iter match {
	      case IterV.Done(a, _) => IterV.Done(s ::: List(a), IterV.El(e))
	      case IterV.Cont(k) => for {
	        h <- k(IterV.El(e))
	        t <- repeatList(iter)
	      } yield s ++ (h :: t)
	    }
	  }
	  IterV.Cont(step(Nil))
	}

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      // use file-based database
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

      // get the head of the query results for a Person
      val row = PersonRowAccess -||> IterV.head

      // Get a List of lists of people grouped by name.
      val groupedByName = PersonRowAccess -||> repeatList(groupBy((p1: Person, p2: Person) => p1.name == p2.name))

      // initialise data
      setupData commitRollbackClose connection printStackTraceOr (n => println(n + " rows affected"))

      // get result and close connection
      val firstPerson = (row <|- "SELECT * FROM PERSON") finalyClose connection

      // get result and close connection
      val adjacentPerson = (groupedByName <|- "SELECT * FROM PERSON") finalyClose connection

      // print the first person result
      firstPerson.println

      // print the first pair of person result
      adjacentPerson.println
    }
  }
}
