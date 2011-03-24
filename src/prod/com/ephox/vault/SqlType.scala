package com.ephox.vault

import java.sql.Types._

sealed trait SqlType {
  type Type = Int

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
case object ArrayType extends SqlType
case object BigIntType extends SqlType
case object BinaryType extends SqlType
case object BitType extends SqlType
case object BlobType extends SqlType
case object BooleanType extends SqlType
case object CharType extends SqlType
case object ClobType extends SqlType
case object DataLinkType extends SqlType
case object DateType extends SqlType
case object DecimalType extends SqlType
case object DistinctType extends SqlType
case object DoubleType extends SqlType
case object FloatType extends SqlType
case object IntegerType extends SqlType
case object JavaObjectType extends SqlType
case object LongNVarCharType extends SqlType
case object LongVarBinaryType extends SqlType
case object LongVarCharType extends SqlType
case object NCharType extends SqlType
case object NClobType extends SqlType
case object NullType extends SqlType
case object NumericType extends SqlType
case object NVarCharType extends SqlType
case object OtherType extends SqlType
case object RealType extends SqlType
case object RefType extends SqlType
case object RowIdType extends SqlType
case object SmallIntType extends SqlType
case object SqlXmlType extends SqlType
case object StructType extends SqlType
case object TimeType extends SqlType
case object TimestampType extends SqlType
case object TinyIntType extends SqlType
case object VarBinaryType extends SqlType
case object VarCharType extends SqlType

trait SqlTypes {
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
}