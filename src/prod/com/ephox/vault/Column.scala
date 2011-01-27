package com.ephox.vault

import java.util.Date
import java.sql.{Timestamp, Time}

trait Column {
  def fold[A](
    nul: => A,
    boolean: Boolean => A,
    string: String => A,
    int: Int => A,
    bigint: BigInt => A,
    float: Float => A,
    double: Double => A,
    bigdecimal: BigDecimal => A,
    time: Time => A,
    timestamp: Timestamp => A,
    date: Date => A): A
}

object Column {
  def nul: Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = nul
  }

  def boolean(value: Boolean): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = boolean(value)
  }

  def string(value: String): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = string(value)
  }

  def int(value: Int): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = int(value)
  }

  def bigint(value: BigInt): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = bigint(value)
  }

  def float(value: Float): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = float(value)
  }

  def double(value: Double): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = double(value)
  }

  def bigdecimal(value: BigDecimal): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = bigdecimal(value)
  }

  def time(value: Time): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = time(value)
  }

  def timestamp(value: Timestamp): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = timestamp(value)
  }

  def date(value: Date): Column = new Column {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      int: Int => A,
      bigint: BigInt => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      time: Time => A,
      timestamp: Timestamp => A,
      date: Date => A): A = date(value)
  }
}
