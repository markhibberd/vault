package vault

import scalaz._, Scalaz._

object GenerateFromDb extends App {
  val params =
    List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')

  def tcontext(n: Int) =
    (0 until n).map(params.apply).map(_ + ": FromDb").mkString(", ")

  def ttuple(n: Int) =
    "(" + (0 until n).map(params.apply).mkString(", ") + ")"

  def paired(n: Int) =
    (0 until n).map(params.apply).map(c => s"of[$c]").mkString(" |+| ")

  def tvalues(n: Int) =
    (0 until n).map(params.apply).map(c => c.toLower).mkString(", ")


  def template(n: Int) = s"""
  implicit def FromDbTuple${n}[${tcontext(n)}]: FromDb[${ttuple(n)}] = for {
${body(n)}
  } yield (${tvalues(n)})
"""

  def body(n: Int) =
    (0 until n).map(params.apply).map(c => s"    ${c.toLower} <- of[${c}]").mkString("\n")

  val rest = (2 to 22).map(template).mkString

  val header =
    s"""package vault
       |
       |trait GeneratedFromDb { this: FromDb.type  =>""".stripMargin

  def save(s: String, c: String): Unit =
    new java.io.PrintWriter(s) <| { _.print(c) } <| { _.close }

  save("src/main/scala/vault/GeneratedFromDb.scala", (header + rest + "\n}\n"))
}
