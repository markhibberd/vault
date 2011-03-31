package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends Querys
  with JDBCTypes
  with KeyX
  with Keyeds
  with KeyedWX
