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
 * `LONGVARBINARY`
 */
case object LongVarBinaryType extends SqlType

/**
 * `LONGVARCHAR`
 */
case object LongVarCharType extends SqlType

/**
 * `NULL`
 */
case object NullType extends SqlType

/**
 * `NUMERIC`
 */
case object NumericType extends SqlType


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
 * `SMALLINT`
 */
case object SmallIntType extends SqlType

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

// -- JDBC 4.0 disabled for the time being --
//
//**
// * `LONGNVARCHAR`
// */
//case object LongNVarCharType extends SqlType
//
///**
// * `NCHAR`
// */
//case object NCharType extends SqlType
//
///**
// * `NCLOB`
// */
//case object NClobType extends SqlType
//
///**
// * `NVARCHAR`
// */
//case object NVarCharType extends SqlType
//
//
///**
// * `ROWID`
// */
//case object RowIdType extends SqlType
//
///**
// * `SQLXML`
// */
//case object SqlXmlType extends SqlType


trait SqlTypes {
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

// -- JDBC 4.0 disabled for the time being --
//
//                  , LongNVarCharType
//                  , NCharType
//                  , NClobType
//                  , NVarCharType
//                  , RowIdType
//                  , SqlXmlType
//

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

//   -- JDBC 4.0 disabled for the time being --
//
//    case LONGNVARCHAR => Some(LongNVarCharType)
//    case NCHAR => Some(NCharType)
//    case NVARCHAR => Some(NVarCharType)
//    case NCLOB => Some(NClobType)
//    case SQLXML => Some(SqlXmlType)
//    case ROWID => Some(RowIdType)

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
