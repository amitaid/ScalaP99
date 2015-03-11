def not(p: Boolean) = p match {
  case true => false
  case false => true
}
def and(p: Boolean, q: Boolean) = (p, q) match {
  case (true, true) => true
  case _ => false
}
def or(p: Boolean, q: Boolean) = (p, q) match {
  case (false, false) => false
  case _ => true
}
def nand(p: Boolean, q: Boolean) = not(and(p, q))
def nor(p: Boolean, q: Boolean) = not(or(p, q))
def xor(p: Boolean, q: Boolean) = or(and(p, not(q)), and(not(p), q))
def impl(p: Boolean, q: Boolean) = or(not(p), q)
val bool2 = List(false, true) flatMap (p => List(false, true) map ((p, _)))
def table2(f: (Boolean, Boolean) => Boolean): Unit = {
  println("A     B     result")
  for (a <- List(false, true); b <- List(false, true))
    printf("%-5s %-5s %-5s\n", a, b, f(a, b))
}
table2(and)

def gray(n: Int): List[String] =
  if (n <= 0) List("")
  else {
    val res = gray(n - 1)
    res.map("0" ++ _) ::: res.reverse.map("1" ++ _.reverse)
  }
gray(3)
