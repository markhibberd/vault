import scalaz._, Scalaz._, Free._
package object vault {
  type FreeDb[+A] = Free[DbValue, A]
}
