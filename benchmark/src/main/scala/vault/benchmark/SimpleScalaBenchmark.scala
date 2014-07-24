package vault.benchmark

import com.google.caliper._

/* SimpleScalaBenchmark template: https://github.com/sirthias/scala-benchmarking-template, Apache 2 License */
trait SimpleScalaBenchmark extends SimpleBenchmark {
  def repeat[@specialized A](reps: Int)(snippet: => A): A = {
    val zero = 0.asInstanceOf[A]
    var i = 0
    var result = zero
    while (i < reps) {
      val res = snippet
      if (res != zero) result = res // make result depend on the benchmarking snippet result
      i = i + 1
    }
    result
  }
}
