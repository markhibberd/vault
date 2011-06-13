package com.ephox.vault

import scalaz._, Scalaz._

object Vault
  extends StringQuerys
  with PossiblyNulls
  with PreparedStatementWs
  with PreparedStatementContexts
  with RowValues
  with RowAccesss
  with RowConnects
  with RowQueryConnects
  with SqlValues
  with SqlAccesss
  with SqlConnects
  with Sqls
  with JDBCTypes
  with SqlTypes
  with Updates
  with Keys
  with Keyeds
  with Mergers
  with SqlExceptionContexts
  with VaultIteratees