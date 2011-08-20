package com.ephox.vault

import scalaz._, Scalaz._

object Vault
  extends Condenses
  with JDBCTypes
  with Keys
  with Keyeds
  with ListWrap
  with Mergers
  with PossiblyNulls
  with PreparedStatementContexts
  with PreparedStatementWs
  with RowAccesss
  with RowConnects
  with RowQueryConnects
  with RowValues
  with SqlAccesss
  with SqlConnects
  with SqlExceptionContexts
  with Sqls
  with SqlValues
  with SqlTypes
  with StringQuerys
  with Updates
  with VaultIteratees