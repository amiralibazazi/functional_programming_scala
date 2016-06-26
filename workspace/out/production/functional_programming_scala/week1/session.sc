object session {
  //CONDITIONALS AND BLOCKS
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x))

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) < (0.001 * x)

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(3)

  sqrt(1e-6)

  //TAIL RECURSION
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  //non-tail recursive
  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)

  def factorialTailRec(n: Int): Int = {
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }
    loop(1, n)
  }

  factorialTailRec(4)
}