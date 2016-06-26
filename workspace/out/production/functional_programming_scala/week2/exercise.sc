object exercise {

  //##############2.2###################

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sum(f)(a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a+1, b)
  }

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  def productMapReduced(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
  //sum of squares
  product(x => x * x)(3, 4)

  def fact(n: Int): Int = productMapReduced(x => x)(1, n)
  fact(5)

  //##############2.5####################

  class Rational(x: Int, y: Int) {
    require(y != 0, "Denominator must be nonzero")

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)

    val numer = x / g
    val denom = y / g

    def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (this.less(that)) that else this

    def neg() = new Rational(-this.numer, this.denom)

    def add(that: Rational) =
      new Rational(
        this.numer * that.denom + that.numer * this.denom,
        this.denom * that.denom)

    def sub(that: Rational) = this.add(that.neg())

    def mul(that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

    override def toString = this.numer + "/" + this.denom
  }

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.add(y)
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)
  x.max(y)

  val strange = new Rational(1, 0)
  strange.add(strange)
}