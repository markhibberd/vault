package com.ephox.vault
package j

import com.ephox.vault.Vault

object Keys {
  def key(id: Long) = Vault.key(id)

  def nokey = Vault.nokey
}