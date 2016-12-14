package isbn

object Isbn {
  def isIsbn13(s: String) = keepValidChars(s) { digits: String =>
    val foundCheckDigit = digits.last.toString
    val expectedCheckDigit = calculateCheckDigit(digits.take(12).toString).toString
    foundCheckDigit == expectedCheckDigit
  }

  def keepValidChars(s: String)(f: String => Boolean): Boolean = {
    val removedHyphensAndSpaces = s.filterNot(c => isHyphen(c) || isSpace(c))
    if (removedHyphensAndSpaces.length == 13 && removedHyphensAndSpaces.forall(_.isDigit)) {
      f(removedHyphensAndSpaces)
    } else {
      false
    }
  }

  def calculateCheckDigit(n: String): Int = {
    val sum = n.zipWithIndex.foldLeft(0) {
      case (acc, (current, index)) if index % 2 == 0 =>
        acc + current.toString.toInt * 1
      case (acc, (current, _)) =>
        acc + current.toString.toInt * 3
    }
    (10 - (sum % 10)) % 10
  }

  def iterateOverDigits(s: Long) = {
    def go(next: Long, digits: List[Long]): Seq[Long] = {
      if (next != 0) {
        val reminder = next % 10
        go(next / 10, reminder :: digits)
      } else {
        digits
      }
    }
    go(s, List())
  }

  def isHyphen(c: Char) = c == '-'
  def isSpace(c: Char) = c == ' '

}
