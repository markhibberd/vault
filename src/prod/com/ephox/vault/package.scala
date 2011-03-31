package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends RowQueryConnects
  with SqlQueryConnects
  with Querys
  with JDBCTypes
  with SqlTypes
  with KeyX
  with Keyeds
  with KeyedWX
  with Mergers
  with VaultIteratees
