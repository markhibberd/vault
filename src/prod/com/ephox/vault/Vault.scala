package com.ephox.vault

import scalaz._, Scalaz._

object Vault
  extends StringQuerys
  with PossiblyNulls
  with PreparedStatementWs
  with RowValues
  with RowAccesss
  with RowConnects
  with RowQueryConnects
  with SqlValues
  with SqlAccesss
  with SqlConnects
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