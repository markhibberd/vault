package vault

import scalaz._, Scalaz._

object GenerateToDb extends App {
  val params =
    List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')

  def tcontext(n: Int) =
    (0 until n).map(params.apply).map(_ + ": ToDb").mkString(", ")

  def ttuple(n: Int) =
    "(" + (0 until n).map(params.apply).mkString(", ") + ")"

  def paired(n: Int) =
    (0 until n).map(params.apply).map(c => s"of[$c]").mkString(" |+| ")

  def tvalues(n: Int) =
    (0 until n).map(params.apply).map(c => c.toLower).mkString(", ")

  def ltvalues(n: Int) = {
    val all = (0 until n).map(params.apply).map(c => c.toLower)
    (1 until n).map(_ => "(").mkString + all.head + ", " + all.tail.mkString("), ") + ")"
  }

def template(n: Int) = s"""
  implicit def ToDbTuple${n}[${tcontext(n)}]: ToDb[${ttuple(n)}] =
    (${paired(n)}).contramap({
      case (${tvalues(n)}) => ${ltvalues(n)}
    })
"""
  val rest = (2 to 22).map(template).mkString

  val header =
    s"""package vault
       |
       |trait GeneratedToDb { this: ToDb.type  =>""".stripMargin

  def save(s: String, c: String): Unit =
    new java.io.PrintWriter(s) <| { _.print(c) } <| { _.close }

  save("src/main/scala/vault/GeneratedToDb.scala", (header + rest + "\n}\n"))
}
