package com.ephox.vault.j

import com.ephox.vault.Vault

object Keys {
  def key(id: Long) = Vault.key(id)

  def nokey = Vault.nokey
}