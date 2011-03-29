package com.ephox.vault;

class VaultException(msg: String, t: Throwable) extends RuntimeException(msg, t) {
  def this(msg: String) = this(msg, null)
  def this(t: Throwable) = this("", t)
}