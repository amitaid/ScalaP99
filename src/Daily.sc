
object Dates {
  val MONTHS = Map(1 -> "January", 2 -> "February", 3 -> "March",
    4 -> "April", 5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August",
    9 -> "September", 10 -> "October", 11 -> "November", 12 -> "December")

  def suffix(day: Int) =
    if (day > 10 && day < 20) "th"
    else day % 10 match {
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => "th"
    }

  def range(date1: String, date2: String): String = {
    val d1 = date1.split("-")
    val d2 = date2.split("-")
    val (year1, month1, day1) = (d1(0), MONTHS(d1(1).toInt), d1(2).toInt)
    val (year2, month2, day2) = (d2(0), MONTHS(d2(1).toInt), d2(2).toInt)

    val day1Str = day1 + suffix(day1)
    val day2Str = day2 + suffix(day2)

    val year1Str = if (year1 == year2) "" else ", " + year1
    val year2Str = if (year1 == year2) "" else ", " + year2

    val month1Str = month1
    val month2Str = if (month1 == month2) "" else month2 + " "

    s"$month1Str $day1Str$year1Str - $month2Str$day2Str$year2Str"
  }

  def main(args:Array[String]) = {
    println(range(args(1), args(2)))
  }
}
Dates.range("2080-07-01", "2080-08-58")
