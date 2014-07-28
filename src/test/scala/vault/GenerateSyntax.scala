package vault

import scalaz._, Scalaz._

object GenerateSyntax extends App {
  val params =
    List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')

  def tcontext(n: Int) =
    if (n == 0) "" else "[" + (0 until n).map(params.apply).map(_ + ": ToDb").mkString(", ") + "]"

  def ttuple(n: Int) =
    (0 until n).map(params.apply).map(_.toLower).mkString(", ")

  def tparams(n: Int) =
    (0 until n).map(params.apply).map(x => x.toLower + ": " + x).mkString(", ")

  def arities(n: Int) =
    "List(" + (0 until n).map(params.apply).map(x => s"ToDb.of[$x].arity").mkString(", ") + ")"

def template(n: Int) = s"""
    def q${tcontext(n)}(${tparams(n)}) = query((${ttuple(n)}), ${arities(n)})
"""
  val rest = (0 to 22).map(template).mkString

  val header =
    s"""package vault
       |object Syntax {
       |  implicit class QueryInterpolator(val sc: StringContext) {
       |
       |    private def query[A: ToDb](a: A, n: List[Int]) = Query((sc.parts zip (n :+ 0)).map({ case (s, i) => s + (1 to i).map(_ => "?").mkString(", ") }).mkString, a)
       |
       |""".stripMargin

  def save(s: String, c: String): Unit =
    new java.io.PrintWriter(s) <| { _.print(c) } <| { _.close }

  save("src/main/scala/vault/Syntax.scala", (header + rest + "\n  }\n}\n"))
}
