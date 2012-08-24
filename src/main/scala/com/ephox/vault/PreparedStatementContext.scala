package com.ephox
package vault

import java.sql.PreparedStatement
import scalaz._, Scalaz._

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JDBCType, Int)]
}

object PreparedStatementContext extends PreparedStatementContextFunctions {
  def apply(s: PreparedStatement, p: Option[(JDBCType, Int)]): PreparedStatementContext =
    new PreparedStatementContext {
      val preparedStatement = s
      val parameters = p
    }
}

trait PreparedStatementContextFunctions {
  val contextPreparedStatementL: PreparedStatementContext @> PreparedStatement =
    Lens(s => Store(PreparedStatementContext(_, s.parameters), s.preparedStatement))

  val contextParametersL: PreparedStatementContext @> Option[(JDBCType, Int)] =
    Lens(s => Store(PreparedStatementContext(s.preparedStatement, _), s.parameters))

  val contextParametersPL: PreparedStatementContext @?> (JDBCType, Int) =
    ~contextParametersL >=> PLensT.somePLens

  val contextParameterTypePL: PreparedStatementContext @?> JDBCType =
    contextParametersPL >=> ~LensT.firstLens

  val contextParameterPositionPL: PreparedStatementContext @?> Int =
    contextParametersPL >=> ~LensT.secondLens

}
