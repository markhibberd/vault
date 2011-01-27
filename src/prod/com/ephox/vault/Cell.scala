package com.ephox.vault

import java.util.Date
import java.sql.{Timestamp, Time}
import java.math.BigDecimal

trait Cell {
  def fold[A](
    nul: => A,
    boolean: Boolean => A,
    string: String => A,
    byte: Byte => A,
    short: Short => A,
    int: Int => A,
    long: Long => A,
    float: Float => A,
    double: Double => A,
    bigdecimal: BigDecimal => A,
    date: Date => A,
    time: Time => A,
    timestamp: Timestamp => A): A
}

object Cell {
  import scalaz._
  import Scalaz._

  def nul: Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = nul
  }

  def boolean(value: Boolean): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = boolean(value)
  }


  def string(value: String): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = string(value)
  }

  def byte(value: Byte): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = byte(value)
  }

  def short(value: Short): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = short(value)
  }

  def int(value: Int): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = int(value)
  }

  def long(value: Long): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = long(value)
  }

  def float(value: Float): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = float(value)
  }

  def double(value: Double): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = double(value)
  }

  def bigdecimal(value: BigDecimal): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = bigdecimal(value)
  }

  def date(value: Date): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = date(value)
  }

  def time(value: Time): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = time(value)
  }

  def timestamp(value: Timestamp): Cell = new Cell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = timestamp(value)
  }

  def oBoolean(value: Option[Boolean]): Cell = value.fold(boolean(_), nul)
  def oString(value: Option[String]): Cell = value.fold(string(_), nul)
  def oByte(value: Option[Byte]): Cell = value.fold(byte(_), nul)
  def oShort(value: Option[Short]): Cell = value.fold(short(_), nul)
  def oInt(value: Option[Int]): Cell = value.fold(int(_), nul)
  def oLong(value: Option[Long]): Cell = value.fold(long(_), nul)
  def oFloat(value: Option[Float]): Cell = value.fold(float(_), nul)
  def oDouble(value: Option[Double]): Cell = value.fold(double(_), nul)
  def oBigDecimal(value: Option[BigDecimal]): Cell = value.fold(bigdecimal(_), nul)
  def oDate(value: Option[Date]): Cell = value.fold(date(_), nul)
  def oTime(value: Option[Time]): Cell = value.fold(time(_), nul)
  def oTimestamp(value: Option[Timestamp]): Cell = value.fold(timestamp(_), nul)

}
