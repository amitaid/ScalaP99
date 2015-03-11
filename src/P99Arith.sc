def isPrime(i: Int): Boolean =
  (i > 1) &&
    (Stream.cons(2, Stream.from(3, 2))
      takeWhile { _ <= Math.sqrt(i) } forall { i % _ != 0 })
isPrime(7)
isPrime(3)
isPrime(200)
def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
gcd(5, 200)
gcd(300, 200)
gcd(575435348, 45343256)
gcd(35, 64)
def coprime(a: Int, b: Int): Boolean = gcd(a, b) == 1
coprime(35,64)
def totient(n: Int): Int = Stream.from(1) takeWhile { _ < n } count { coprime(_, n) }
totient(10)
def primeFactors(n: Int): List[Int] = {
  def primeFactorsRec(n: Int, ls: List[Int]): List[Int] =
    if (n == 1) ls
    else {
      val start = if (ls.isEmpty) 2 else ls.head
      val l = ((start to n) find (n % _ == 0)).get
      primeFactorsRec(n / l, l :: ls)
    }
  primeFactorsRec(n, List()).reverse
}
primeFactors(64)
def primeFactorMultiplicity(n: Int): Map[Int, Int] = {
  def pfmRec(ls: List[Int]): Map[Int, Int] = ls match {
    case Nil => Map()
    case x :: xs => Map(x -> ls.takeWhile(_ == x).length) ++ pfmRec(ls.dropWhile(_ == x))
  }
  pfmRec(primeFactors(n))
}
primeFactorMultiplicity(64)
primeFactorMultiplicity(55)


def totientImproved(n: Int): Int =
  primeFactorMultiplicity(n).foldLeft(1)((acc, kv) => acc * (kv._1 - 1) * math.pow(kv._1, kv._2 - 1).toInt)
totientImproved(10)

def listPrimesInRange(i: Int, j: Int): List[Int] =
  Stream.from({if (i % 2 != 0) i else i+1}, 2) takeWhile(_ <= j) filter isPrime toList

listPrimesInRange(7, 31)

def goldbach(n: Int): (Int, Int) = {
  val num = listPrimesInRange(2, n - 1).find(i => isPrime(n - i)).get
  (num, n - num)
}
goldbach(28)

def listGoldbach(r: Range): Unit = {
  r foreach (n => if (n % 2 == 0) {
    val (i, j) = goldbach(n)
    val pattern = s"$n = $i + $j"
    println(pattern)
  })
}
listGoldbach(9 to 20)