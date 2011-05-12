package com.ephox.vault

import java.sql.Types._

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
  def toType: Type = this match {
    case ArrayType => ARRAY
    case BigIntType => BIGINT
    case BinaryType => BINARY
    case BitType => BIT
    case BlobType => BLOB
    case BooleanType => BOOLEAN
    case CharType => CHAR
    case ClobType => CLOB
    case DataLinkType => DATALINK
    case DateType => DATE
    case DecimalType => DECIMAL
    case DistinctType => DISTINCT
    case DoubleType => DOUBLE
    case FloatType => FLOAT
    case IntegerType => INTEGER
    case JavaObjectType => JAVA_OBJECT
    case LongNVarCharType => LONGNVARCHAR
    case LongVarBinaryType => LONGVARBINARY
    case LongVarCharType => LONGVARCHAR
    case NCharType => NCHAR
    case NClobType => NCLOB
    case NullType => NULL
    case NumericType => NUMERIC
    case NVarCharType => NVARCHAR
    case OtherType => OTHER
    case RealType => REAL
    case RefType => REF
    case RowIdType => ROWID
    case SmallIntType => SMALLINT
    case SqlXmlType => SQLXML
    case StructType => STRUCT
    case TimeType => TIME
    case TimestampType => TIMESTAMP
    case TinyIntType => TINYINT
    case VarBinaryType => VARBINARY
    case VarCharType => VARCHAR
  }
}

/**
 * `ARRAY`
 */
case object ArrayType extends SqlType

/**
 * `BIGINT`
 */
case object BigIntType extends SqlType

/**
 * `BINARY`
 */
case object BinaryType extends SqlType

/**
 * `BIT`
 */
case object BitType extends SqlType

/**
 * `BLOB`
 */
case object BlobType extends SqlType

/**
 * `BOOLEAN`
 */
case object BooleanType extends SqlType

/**
 * `CHAR`
 */
case object CharType extends SqlType

/**
 * `CLOB`
 */
case object ClobType extends SqlType

/**
 * `DATALINK`
 */
case object DataLinkType extends SqlType

/**
 * `DATE`
 */
case object DateType extends SqlType

/**
 * `DECIMAL`
 */
case object DecimalType extends SqlType

/**
 * `DISTINCT`
 */
case object DistinctType extends SqlType

/**
 * `DOUBLE`
 */
case object DoubleType extends SqlType

/**
 * `FLOAT`
 */
case object FloatType extends SqlType

/**
 * `INTEGER`
 */
case object IntegerType extends SqlType

/**
 * `JAVA_OBJECT`
 */
case object JavaObjectType extends SqlType

/**
 * `LONGNVARCHAR`
 */
case object LongNVarCharType extends SqlType

/**
 * `LONGVARBINARY`
 */
case object LongVarBinaryType extends SqlType

/**
 * `LONGVARCHAR`
 */
case object LongVarCharType extends SqlType

/**
 * `NCHAR`
 */
case object NCharType extends SqlType

/**
 * `NCLOB`
 */
case object NClobType extends SqlType

/**
 * `NULL`
 */
case object NullType extends SqlType

/**
 * `NUMERIC`
 */
case object NumericType extends SqlType

/**
 * `NVARCHAR`
 */
case object NVarCharType extends SqlType

/**
 * `OTHER`
 */
case object OtherType extends SqlType

/**
 * `REAL`
 */
case object RealType extends SqlType

/**
 * `REF`
 */
case object RefType extends SqlType

/**
 * `ROWID`
 */
case object RowIdType extends SqlType

/**
 * `SMALLINT`
 */
case object SmallIntType extends SqlType

/**
 * `SQLXML`
 */
case object SqlXmlType extends SqlType

/**
 * `STRUCT`
 */
case object StructType extends SqlType

/**
 * `TIME`
 */
case object TimeType extends SqlType

/**
 * `TIMESTAMP`
 */
case object TimestampType extends SqlType

/**
 * `TINYINT`
 */
case object TinyIntType extends SqlType

/**
 * `VARBINARY`
 */
case object VarBinaryType extends SqlType

/**
 * `VARCHAR`
 */
case object VarCharType extends SqlType

trait SqlTypes {
  /**
   * All possible generic SQL types.
   */
  val sqlTypes = Set(
                      ArrayType
                    , BigIntType
                    , BinaryType
                    , BitType
                    , BlobType
                    , BooleanType
                    , CharType
                    , ClobType
                    , DataLinkType
                    , DateType
                    , DecimalType
                    , DistinctType
                    , DoubleType
                    , FloatType
                    , IntegerType
                    , JavaObjectType
                    , LongNVarCharType
                    , LongVarBinaryType
                    , LongVarCharType
                    , NCharType
                    , NClobType
                    , NullType
                    , NumericType
                    , NVarCharType
                    , OtherType
                    , RealType
                    , RefType
                    , RowIdType
                    , SmallIntType
                    , SqlXmlType
                    , StructType
                    , TimeType
                    , TimestampType
                    , TinyIntType
                    , VarBinaryType
                    , VarCharType
                    )

  /**
   * Construct a generic SQL type from a value used in [[java.sql.Types]].
   *
   * @param n The value used in [[java.sql.Types]].
   */
  def sqlTypeFromInt(n: Int): Option[SqlType] = n match {
    case ARRAY => Some(ArrayType)
    case BIGINT => Some(BigIntType)
    case BINARY => Some(BinaryType)
    case BIT => Some(BitType)
    case BLOB => Some(BlobType)
    case BOOLEAN => Some(BooleanType)
    case CHAR => Some(CharType)
    case CLOB => Some(ClobType)
    case DATALINK => Some(DataLinkType)
    case DATE => Some(DateType)
    case DECIMAL => Some(DecimalType)
    case DISTINCT => Some(DistinctType)
    case DOUBLE => Some(DoubleType)
    case FLOAT => Some(FloatType)
    case INTEGER => Some(IntegerType)
    case JAVA_OBJECT => Some(JavaObjectType)
    case LONGNVARCHAR => Some(LongNVarCharType)
    case LONGVARBINARY => Some(LongVarBinaryType)
    case LONGVARCHAR => Some(LongVarCharType)
    case NCHAR => Some(NCharType)
    case NCLOB => Some(NClobType)
    case NULL => Some(NullType)
    case NUMERIC => Some(NumericType)
    case NVARCHAR => Some(NVarCharType)
    case OTHER => Some(OtherType)
    case REAL => Some(RealType)
    case REF => Some(RefType)
    case ROWID => Some(RowIdType)
    case SMALLINT => Some(SmallIntType)
    case SQLXML => Some(SqlXmlType)
    case STRUCT => Some(StructType)
    case TIME => Some(TimeType)
    case TIMESTAMP => Some(TimestampType)
    case TINYINT => Some(TinyIntType)
    case VARBINARY => Some(VarBinaryType)
    case VARCHAR => Some(VarCharType)
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
