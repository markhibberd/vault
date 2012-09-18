package com.ephox
package vault
package sql

sealed trait Incompatibility {
  // JDBC uses Int everywhere, what was the incompatible assumption?
  val int: Int

  // What is the URL documenting the assumption?
  val apiUrl: String

  // What is the message specifying the assumption?
  val message: String
}

object Incompatibility {
  def apply(i: Int, a: String, m: String): Incompatibility =
    new Incompatibility {
      val int = i
      val apiUrl = a
      val message = m
    }
}
