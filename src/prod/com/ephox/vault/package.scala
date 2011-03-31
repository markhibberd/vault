package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends RowQueryConnects
  with SqlQueryConnects
  with Querys
  with JDBCTypes
  with KeyX
  with Keyeds
  with KeyedWX
