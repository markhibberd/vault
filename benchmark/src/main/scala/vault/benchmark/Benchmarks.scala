package vault.benchmark

import com.google.caliper._

object PlaceholderBenchApp extends App {
  Runner.main(classOf[PlaceholderBench], args)
}

case class PlaceholderBench() extends SimpleScalaBenchmark {
  def time_addition(n: Int) =
    repeat(n) {
     (1 to n).toList.reduceLeft(_ + _)
    }

  def time_subtraction(n: Int) =
    repeat(n) {
     (1 to n).toList.reduceLeft(_ - _)
    }
}
