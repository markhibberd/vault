package vault

import scalaz._, Scalaz._

case class Db[A, +B](run: ReaderWriterDbValue[A, B])
