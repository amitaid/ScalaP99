
import scala.util.Random

// 1
def last(list: List[_]) = list.takeRight(1).head
last(List(1, 2, 3, 4, 5))

// 2
def penultimate(list: List[_]) = list.takeRight(2).head
penultimate(List(1, 2, 3, 4, 5))

// 3
def nth(n: Int, list: List[_]): Any = n match {
  case 0 => list.head
  case _ => nth(n - 1, list.drop(1))
}
nth(1, List(1, 2, 3, 4, 5))
nth(3, List(1, 2, 3, 4, 5))

// 4
def length(list: List[_]) = list.length
length(List(1, 2, 3))

// 5
def reverse(list: List[_]): List[_] = list match {
  case Nil => list
  case x :: Nil => list
  case x :: xs => reverse(xs) ++ List(x)
}
reverse(List(1, 2, 3))

// 6
def palindrome(list: List[_]): Boolean = list match {
  case Nil => true
  case x :: Nil => true
  case x :: xs => x == xs.takeRight(1).head && palindrome(xs.dropRight(1))
}
palindrome(List(1, 2, 3))
palindrome(List(1, 2, 1))
palindrome(List(1, 1))
palindrome(List(1))
palindrome(List())

// 7
def flatten(list: List[_]): List[_] = list match {
  case Nil => list
  case x :: xs => x match {
    case ms: List[_] => flatten(ms) ++ flatten(xs)
    case y => y :: flatten(xs)
  }
}
flatten(List(List(1, 2), 3, List(4), List(List(5, 6))))

// 8
def compress(list: List[_]): List[_] = list match {
  case Nil => list
  case x :: Nil => list
  case x :: y :: xs => if (x == y) compress(y :: xs) else x :: compress(y :: xs)
}
compress(List(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 3))

def pack(list: List[_]): List[_] = {
  if (list.isEmpty) List()
  else {
    val elem = list.takeWhile(_ == list.head)
    elem :: pack(list.drop(elem.length))
  }
}
pack(List(1, 1, 1, 1, 2, 2, 2, 3, 3, 1, 1))

// 10
def encode(list: List[_]): List[(Int, _)] = list match {
  case Nil => Nil
  case _ =>
    val value = list.head
    val count = list.takeWhile(_ == value).length
    (count, value) :: encode(list.drop(count))
}
encode(List('1', '1', '1', '2', '2', '3', '3', '3', '3', '4', '3'))
// 11
def encodeModified(list: List[_]): List[_] = encode(list) map {
  case (1, c) => c
  case (n, c) => (n, c)
}
encodeModified(List('1', '1', '1', '2', '2', '3', '3', '3', '3', '4', '3'))
// 12
def decode(list: List[(Int, _)]): List[_] = list match {
  case Nil => list
  case (1, x) :: xs => x :: decode(xs)
  case (n: Int, x) :: xs => x :: decode((n - 1, x) :: xs)
}
decode(List((3, 1), (2, 2), (4, 3), (1, 4), (1, 3)))

def duplicate(list: List[_]): List[_] = list match {
  case Nil => list
  case x :: xs => x :: x :: duplicate(xs)
}
duplicate(List(1, 2, 3))
def duplicateN(n: Int, list: List[_]): List[_] = list match {
  case Nil => list
  case x :: xs => List.fill(n)(x) ++ duplicateN(n, xs)
}
duplicateN(3, List('a, 'b, 'c))
duplicateN(0, List('a, 'b, 'c))
def drop(n: Int, list: List[_]): List[_] = list match {
  case Nil => list
  case _ => list.take(n - 1) ++ drop(n, list.drop(n))
}
drop(2, List(1, 2, 3, 4, 5, 6, 7, 8, 9))
drop(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9))
def split(n: Int, list: List[_]) = (list.take(n), list.drop(n))
split(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9))
def slice(i: Int, j: Int, ls: List[_]) = ls.drop(i).take(j - (i max 0))
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
def rotate(n: Int, ls: List[_]): List[_] =
  if (n <= 0) ls
  else rotate(n - 1, ls.tail ++ List(ls.head))
rotate(2, List(0, 1, 2, 3, 4, 5))
def removeAt(i: Int, ls: List[_]) = {
  def removeAtRec(j: Int, lst: List[_], res: List[_]): (List[_], Any) =
    if (j == 0) (res ++ lst.tail, lst.head)
    else removeAtRec(j - 1, lst.tail, res ++ List(lst.head))
  removeAtRec(i, ls, List())
}
removeAt(3, List(0, 1, 2, 3, 4, 5, 6))
def insertAt(elem: Any, i: Int, ls: List[_]) = ls.take(i) ++ (elem :: ls.drop(i))
insertAt(5, -1, List(0, 1, 2, 3, 4, 5, 6))
def range(i: Int, j: Int): List[Int] =
  if (i > j) Nil
  else i :: range(i + 1, j)
range(3, 7)
def randomSelect(n: Int, ls: List[_]) = {
  def randomSelectRec(m: Int, lst: List[_], res: List[_], r: Random): (List[_], List[_]) =
    if (m <= 0) (lst, res)
    else {
      val (l, elem) = removeAt(r.nextInt(lst.length), lst)
      randomSelectRec(m - 1, l, elem :: res, r)
    }
  randomSelectRec(n, ls, List(), new Random)
}
randomSelect(3, List(0, 1, 2, 3, 4, 5))
def lotto(n: Int, r: Int) = randomSelect(n, range(0, r))
lotto(5, 30)
def randomPermute(ls: List[_]): List[_] =
  if (ls.length <= 1) ls
  else {
    val (rest, selected) = randomSelect(1, ls)
    selected.head :: randomPermute(rest)
  }
randomPermute(List(1, 2, 3, 4, 5, 6, 7))
def combinations(n: Int, ls: List[_]): List[List[_]] =
  if (ls.isEmpty) List()
  else if (n <= 0) List(List())
  else if (ls.length == n) List(ls)
  else (combinations(n - 1, ls.tail) map (l => ls.head :: l)) ++ combinations(n, ls.tail)
combinations(4, List(0, 1, 2, 3, 4, 5))

def combiRest(m: Int, l: List[_]): List[(List[_], List[_])] = {
  def combiRestRec(n: Int, ls: List[_], rest: List[_]): List[(List[_], List[_])] =
    if (ls.isEmpty) List()
    else if (n <= 0) List((List(), ls ++ rest))
    else if (ls.length == n) List((ls, rest))
    else (combiRestRec(n - 1, ls.tail, rest) map { case (l1, l2) => (ls.head :: l1, l2)}) ++
      combiRestRec(n, ls.tail, ls.head :: rest)
  combiRestRec(m, l, List())
}
def group(sizes: List[Int], ls: List[_]): List[List[List[_]]] =
  if (sizes.length == 1) List(List(ls))
  else {
    val first = combiRest(sizes.head, ls)
    first flatMap {
      case (selected, rest) => group(sizes.tail, rest) map (l => selected :: l)
    }
  }
group(List(1, 2, 1), List('a, 'b, 'c, 'd))


