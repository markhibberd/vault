package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends StringQuerys
  with PreparedStatementWs
  with RowValues
  with RowConnects
  with RowQueryConnects
  with SqlAccesss
  with SqlQueryConnects
  with Querys
  with JDBCTypes
  with SqlTypes
  with Updates
  with KeyX
  with Keyeds
  with KeyedWX
  with Mergers
  with VaultIteratees
