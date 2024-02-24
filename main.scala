
object SumCalculator {
  def main(args: Array[String]): Unit = {
    var a = 1
    var b = 5
    var result = calculateSum(a, b, (x) => x)
    var result2= calculateSum(a,b,(x)=>x*x)
    var result3= calculateSum(a,b,(x)=>factorial(x))
    println(s"Sum intow $a do $b = $result")
    println(s"Sum kwadratow od $a do $b = $result2")
    println(s"Sum silni od $a do $b = $result3")
    a = 6
    b = 10
    result = calculateSum(a,b,(x=>x))
    result2 = calculateSum(a, b, (x) => x * x)
    result3 = calculateSum(a, b, (x) => factorial(x))
    println(s"Sum intow $a do $b = $result")
    println(s"Sum kwadratow od $a do $b = $result2")
    println(s"Sum silni od $a do $b = $result3")
    a = 2
    b = 9
    result = calculateSum(a, b, (x => x))
    result2 = calculateSum(a, b, (x) => x * x)
    result3 = calculateSum(a, b, (x) => factorial(x))
    println(s"Sum intow $a do $b = $result")
    println(s"Sum kwadratow od $a do $b = $result2")
    println(s"Sum silni od $a do $b = $result3")
  }

  def calculateSum(a: Int, b: Int, f: Int => Int): Int = {
    @scala.annotation.tailrec
    def applyHelper(current: Int, end: Int, accumulator: Int): Int = {
      if (current > end) accumulator
      else applyHelper(current + 1, end, accumulator + f(current))
    }

    applyHelper(a, b, 0)
  }
}

def factorial(n: Int): Int = {
  @scala.annotation.tailrec
  def factorialHelper(current: Int, acc: Int): Int = {
    if (current <= 1) acc
    else factorialHelper(current - 1, acc * current)
  }

  factorialHelper(n, 1)
}

