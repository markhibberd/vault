package com.ephox.vault2

import java.sql.Connection


sealed trait Connector[M[_], A] {
  val connect: Connection => M[SQLValue[A]]
}