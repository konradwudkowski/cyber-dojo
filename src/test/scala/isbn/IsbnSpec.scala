package isbn

import isbn.Isbn._
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

 /*

 source: http://www.cyber-dojo.org/

 ISBN - International Standard Book Number
 -----------------------------------------
 There are two ISBN standards: ISBN-10 and ISBN-13.
 Support for ISBN-13 is essential, whereas support
 for ISBN-10 is optional.
 Here are some valid examples of each:

 ISBN-10:    0471958697
             0 471 60695 2
             0-470-84525-2
             0-321-14653-0

 ISBN-13:    9780470059029
             978 0 471 48648 0
             978-0596809485
             978-0-13-149505-0
             978-0-262-13472-9

 ISBN-10 is made up of 9 digits plus a check digit (which
 may be 'X') and ISBN-13 is made up of 12 digits plus a
 check digit. Spaces and hyphens may be included in a code,
 but are not significant. This means that 9780471486480 is
 equivalent to 978-0-471-48648-0 and 978 0 471 48648 0.

 The check digit for ISBN-10 is calculated by multiplying
 each digit by its position (i.e., 1 x 1st digit, 2 x 2nd
 digit, etc.), summing these products together and taking
 modulo 11 of the result (with 'X' being used if the result
 is 10).

 The check digit for ISBN-13 is calculated by multiplying
 each digit alternately by 1 or 3 (i.e., 1 x 1st digit,
 3 x 2nd digit, 1 x 3rd digit, 3 x 4th digit, etc.), summing
 these products together, taking modulo 10 of the result
 and subtracting this value from 10, and then taking the
 modulo 10 of the result again to produce a single digit.

 Basic task:
 Create a function that takes a string and returns true
 if that is a valid ISBN-13 and false otherwise.

 Advanced task:
 Also return true if the string is a valid ISBN-10.
 */

class IsbnSpec extends FreeSpec with Matchers with PropertyChecks {

  "ISBN-13" - {
    "function keepValidChars should only allow values that are 13 chars long after discarding whitespace and hyphens" in {
      val _13CharLongNumber = mixWithRandomSpacesAndHyphens(List.fill(13)('1').mkString)
      forAll(_13CharLongNumber) { n =>
        keepValidChars(n)(_ => true) shouldBe true
      }
    }
    "function calculateCheckDigit should calculate a valid check digit" in {
      val number = "111111111111"
      val sum = 1 * 1 +
        1 * 3 +
        1 * 1 +
        1 * 3 +
        1 * 1 +
        1 * 3 +
        1 * 1 +
        1 * 3 +
        1 * 1 +
        1 * 3 +
        1 * 1 +
        1 * 3
      val expectedCheckDigit = (10 - (sum % 10)) % 10
      calculateCheckDigit(number) shouldBe expectedCheckDigit
    }
    "isbn13 should contain only 13 digits plus optional spaces and hyphens" in {
        val input = Table(
          "number"              -> "expected result",
      //==================================================
          "1234567891231"             -> true,
          "123-4567-891-231"          -> true,
          " 123 4 5 67 8 9 1231"      -> true,
          "123 - 456789 - 1231"       -> true,
          "not digits"                -> false,
          "123"                       -> false
        )

        forAll(input) {
          case (number, result) =>
            isIsbn13(number) shouldBe result
        }
    }
  }

  def mixWithRandomSpacesAndHyphens(s: String) = for {
    hyphens <- listOf(const('-'))
    spaces <- listOf(const(' '))
  } yield {
    Random.shuffle(hyphens ::: spaces ::: s.toList).mkString
  }


}
