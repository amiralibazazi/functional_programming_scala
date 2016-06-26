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

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val numer = x
    val denom = y

    def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (this < that) that else this

    def unary_- = new Rational(-this.numer, this.denom)

    def + (that: Rational) =
      new Rational(
        this.numer * that.denom + that.numer * this.denom,
        this.denom * that.denom)

    def - (that: Rational) = this + -that

    def * (that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

    override def toString = {
      val g = gcd(this.numer, this.denom)
      this.numer / g + "/" + this.denom / g
    }
  }

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x + y
  x - y - z
  y + y
  x < y
  x max y

  val two = new Rational(2)

  x + z
}