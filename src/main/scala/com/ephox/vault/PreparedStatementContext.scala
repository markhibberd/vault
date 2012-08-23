package com.ephox
package vault

import java.sql.PreparedStatement
import scalaz._, Scalaz._

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JDBCType, Int)]
}

object PreparedStatementContext extends PreparedStatementContextFunctions

trait PreparedStatementContextFunctions {
  def preparedStatementContext(s: PreparedStatement, p: Option[(JDBCType, Int)]): PreparedStatementContext =
    new PreparedStatementContext {
      val preparedStatement = s
      val parameters = p
    }

  def preparedStatementL: PreparedStatementContext @> PreparedStatement =
    Lens(s => Store(preparedStatementContext(_, s.parameters), s.preparedStatement))

  def contextParametersL: PreparedStatementContext @> Option[(JDBCType, Int)] =
    Lens(s => Store(preparedStatementContext(s.preparedStatement, _), s.parameters))

  def contextParametersPL: PreparedStatementContext @?> (JDBCType, Int) =
    ~contextParametersL >=> PLensT.somePLens

  def contextParameterTypePL: PreparedStatementContext @?> JDBCType =
    contextParametersPL >=> ~LensT.firstLens

  def contextParameterPositionPL: PreparedStatementContext @?> Int =
    contextParametersPL >=> ~LensT.secondLens

}
