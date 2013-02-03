package com.ephox
package vault
package sql

import java.sql.Types._
import scalaz._, Scalaz._

/**
 * All generics SQL types.
 *
 * @see java.sql.Types
 * @author Tony Morris <tmorris@tmorris.net>
 */
sealed trait SqlType {
  /**
   * Used by [[java.sql.Types]] for denoting generic SQL types.
   */
  type Type = Int

  /**
   * Returns the corresponding value used by [[java.sql.Types]] for denoting generic SQL types.
   */
  def int: Type =
    this match {
      case ArraySqlType => ARRAY
      case BigIntSqlType => BIGINT
      case BinarySqlType => BINARY
      case BitSqlType => BIT
      case BlobSqlType => BLOB
      case BooleanSqlType => BOOLEAN
      case CharSqlType => CHAR
      case ClobSqlType => CLOB
      case DataLinkSqlType => DATALINK
      case DateSqlType => DATE
      case DecimalSqlType => DECIMAL
      case DistinctSqlType => DISTINCT
      case DoubleSqlType => DOUBLE
      case FloatSqlType => FLOAT
      case IntegerSqlType => INTEGER
      case JavaObjectSqlType => JAVA_OBJECT
      case LongVarBinarySqlType => LONGVARBINARY
      case LongVarCharSqlType => LONGVARCHAR
      case NullSqlType => NULL
      case NumericSqlType => NUMERIC
      case OtherSqlType => OTHER
      case RealSqlType => REAL
      case RefSqlType => REF
      case SmallIntSqlType => SMALLINT
      case StructSqlType => STRUCT
      case TimeSqlType => TIME
      case TimestampSqlType => TIMESTAMP
      case TinyIntSqlType => TINYINT
      case VarBinarySqlType => VARBINARY
      case VarCharSqlType => VARCHAR

  //    -- JDBC 4.0 disabled for the time being --
  //
  //    case NCharType => NCHAR
  //    case NVarCharType => NVARCHAR
  //    case NClobType => NCLOB
  //    case LongNVarCharType => LONGNVARCHAR
  //    case RowIdType => ROWID
  //    case SqlXmlType => SQLXML

    }

  def ===(t: SqlType): Boolean =
    this == t

  override def toString =
    this match {
      case ArraySqlType => "Array"
      case BigIntSqlType => "BigInt"
      case BinarySqlType => "Binary"
      case BitSqlType => "Bit"
      case BlobSqlType => "Blob"
      case BooleanSqlType => "Boolean"
      case CharSqlType => "Char"
      case ClobSqlType => "Clob"
      case DataLinkSqlType => "DataLink"
      case DateSqlType => "Date"
      case DecimalSqlType => "Decimal"
      case DistinctSqlType => "Distinct"
      case DoubleSqlType => "Double"
      case FloatSqlType => "Float"
      case IntegerSqlType => "Integer"
      case JavaObjectSqlType => "JavaObject"
      case LongVarBinarySqlType => "LongVarBinary"
      case LongVarCharSqlType => "LongVarChar"
      case NullSqlType => "Null"
      case NumericSqlType => "Numeric"
      case OtherSqlType => "Other"
      case RealSqlType => "Real"
      case RefSqlType => "Ref"
      case SmallIntSqlType => "SmallInt"
      case StructSqlType => "Struct"
      case TimeSqlType => "Time"
      case TimestampSqlType => "Timestamp"
      case TinyIntSqlType => "TinyInt"
      case VarBinarySqlType => "VarBinary"
      case VarCharSqlType => "VarChar"
    }
}

/**
 * `ARRAY`
 */
case object ArraySqlType extends SqlType

/**
 * `BIGINT`
 */
case object BigIntSqlType extends SqlType

/**
 * `BINARY`
 */
case object BinarySqlType extends SqlType

/**
 * `BIT`
 */
case object BitSqlType extends SqlType

/**
 * `BLOB`
 */
case object BlobSqlType extends SqlType

/**
 * `BOOLEAN`
 */
case object BooleanSqlType extends SqlType

/**
 * `CHAR`
 */
case object CharSqlType extends SqlType

/**
 * `CLOB`
 */
case object ClobSqlType extends SqlType

/**
 * `DATALINK`
 */
case object DataLinkSqlType extends SqlType

/**
 * `DATE`
 */
case object DateSqlType extends SqlType

/**
 * `DECIMAL`
 */
case object DecimalSqlType extends SqlType

/**
 * `DISTINCT`
 */
case object DistinctSqlType extends SqlType

/**
 * `DOUBLE`
 */
case object DoubleSqlType extends SqlType

/**
 * `FLOAT`
 */
case object FloatSqlType extends SqlType

/**
 * `INTEGER`
 */
case object IntegerSqlType extends SqlType

/**
 * `JAVA_OBJECT`
 */
case object JavaObjectSqlType extends SqlType

/**
 * `LONGVARBINARY`
 */
case object LongVarBinarySqlType extends SqlType

/**
 * `LONGVARCHAR`
 */
case object LongVarCharSqlType extends SqlType

/**
 * `NULL`
 */
case object NullSqlType extends SqlType

/**
 * `NUMERIC`
 */
case object NumericSqlType extends SqlType


/**
 * `OTHER`
 */
case object OtherSqlType extends SqlType

/**
 * `REAL`
 */
case object RealSqlType extends SqlType

/**
 * `REF`
 */
case object RefSqlType extends SqlType

/**
 * `SMALLINT`
 */
case object SmallIntSqlType extends SqlType

/**
 * `STRUCT`
 */
case object StructSqlType extends SqlType

/**
 * `TIME`
 */
case object TimeSqlType extends SqlType

/**
 * `TIMESTAMP`
 */
case object TimestampSqlType extends SqlType

/**
 * `TINYINT`
 */
case object TinyIntSqlType extends SqlType

/**
 * `VARBINARY`
 */
case object VarBinarySqlType extends SqlType

/**
 * `VARCHAR`
 */
case object VarCharSqlType extends SqlType

object SqlType extends SqlTypeFunctions with SqlTypeInstances

trait SqlTypeFunctions {
  /**
   * All possible generic SQL types.
   */
  val sqlTypes = Set(
                      ArraySqlType
                    , BigIntSqlType
                    , BinarySqlType
                    , BitSqlType
                    , BlobSqlType
                    , BooleanSqlType
                    , CharSqlType
                    , ClobSqlType
                    , DataLinkSqlType
                    , DateSqlType
                    , DecimalSqlType
                    , DistinctSqlType
                    , DoubleSqlType
                    , FloatSqlType
                    , IntegerSqlType
                    , JavaObjectSqlType
                    , LongVarBinarySqlType
                    , LongVarCharSqlType
                    , NullSqlType
                    , NumericSqlType
                    , OtherSqlType
                    , RealSqlType
                    , RefSqlType
                    , SmallIntSqlType
                    , StructSqlType
                    , TimeSqlType
                    , TimestampSqlType
                    , TinyIntSqlType
                    , VarBinarySqlType
                    , VarCharSqlType
                    )

  /**
   * Construct a generic SQL type from a value used in [[java.sql.Types]].
   *
   * @param n The value used in [[java.sql.Types]].
   */
  def sqlTypeFromInt(n: Int): Option[SqlType] = n match {
    case ARRAY => Some(ArraySqlType)
    case BIGINT => Some(BigIntSqlType)
    case BINARY => Some(BinarySqlType)
    case BIT => Some(BitSqlType)
    case BLOB => Some(BlobSqlType)
    case BOOLEAN => Some(BooleanSqlType)
    case CHAR => Some(CharSqlType)
    case CLOB => Some(ClobSqlType)
    case DATALINK => Some(DataLinkSqlType)
    case DATE => Some(DateSqlType)
    case DECIMAL => Some(DecimalSqlType)
    case DISTINCT => Some(DistinctSqlType)
    case DOUBLE => Some(DoubleSqlType)
    case FLOAT => Some(FloatSqlType)
    case INTEGER => Some(IntegerSqlType)
    case JAVA_OBJECT => Some(JavaObjectSqlType)
    case LONGVARBINARY => Some(LongVarBinarySqlType)
    case LONGVARCHAR => Some(LongVarCharSqlType)
    case NULL => Some(NullSqlType)
    case NUMERIC => Some(NumericSqlType)
    case OTHER => Some(OtherSqlType)
    case REAL => Some(RealSqlType)
    case REF => Some(RefSqlType)
    case SMALLINT => Some(SmallIntSqlType)
    case STRUCT => Some(StructSqlType)
    case TIME => Some(TimeSqlType)
    case TIMESTAMP => Some(TimestampSqlType)
    case TINYINT => Some(TinyIntSqlType)
    case VARBINARY => Some(VarBinarySqlType)
    case VARCHAR => Some(VarCharSqlType)
    case _ => None
  }

  /**
   * Construct a generic SQL type from a value used in [[java.sql.Types]] or using the given default in the absence of a correspondence.
   *
   * @param n The value used in [[java.sql.Types]].
   * @param t The default value to use if the given [[java.sql.Types]] value is invalid.
   */
  def sqlTypeFromIntOr(n: Int, t: => SqlType): SqlType =
    sqlTypeFromInt(n) getOrElse t

}

trait SqlTypeInstances {
  implicit val SqlTypeInstances: Equal[SqlType] with Show[SqlType] =
    new Equal[SqlType] with Show[SqlType] {
      def equal(a1: SqlType, a2: SqlType) =
        a1 === a2

      override def shows(a: SqlType) =
        a.toString
    }
}