package com.ephox.vault2

import java.sql.Types._

sealed trait SQLType {
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
case object ArrayType extends SQLType
case object BigIntType extends SQLType
case object BinaryType extends SQLType
case object BitType extends SQLType
case object BlobType extends SQLType
case object BooleanType extends SQLType
case object CharType extends SQLType
case object ClobType extends SQLType
case object DataLinkType extends SQLType
case object DateType extends SQLType
case object DecimalType extends SQLType
case object DistinctType extends SQLType
case object DoubleType extends SQLType
case object FloatType extends SQLType
case object IntegerType extends SQLType
case object JavaObjectType extends SQLType
case object LongNVarCharType extends SQLType
case object LongVarBinaryType extends SQLType
case object LongVarCharType extends SQLType
case object NCharType extends SQLType
case object NClobType extends SQLType
case object NullType extends SQLType
case object NumericType extends SQLType
case object NVarCharType extends SQLType
case object OtherType extends SQLType
case object RealType extends SQLType
case object RefType extends SQLType
case object RowIdType extends SQLType
case object SmallIntType extends SQLType
case object SqlXmlType extends SQLType
case object StructType extends SQLType
case object TimeType extends SQLType
case object TimestampType extends SQLType
case object TinyIntType extends SQLType
case object VarBinaryType extends SQLType
case object VarCharType extends SQLType
