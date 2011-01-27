package com.ephox.vault

import java.util.Date
import java.sql.{Timestamp, Time}

trait Cell {
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

object Cell {
  def nul: Cell = new Cell {
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

  def boolean(value: Boolean): Cell = new Cell {
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

  def string(value: String): Cell = new Cell {
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

  def int(value: Int): Cell = new Cell {
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

  def bigint(value: BigInt): Cell = new Cell {
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

  def float(value: Float): Cell = new Cell {
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

  def double(value: Double): Cell = new Cell {
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

  def bigdecimal(value: BigDecimal): Cell = new Cell {
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

  def time(value: Time): Cell = new Cell {
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

  def timestamp(value: Timestamp): Cell = new Cell {
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

  def date(value: Date): Cell = new Cell {
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
