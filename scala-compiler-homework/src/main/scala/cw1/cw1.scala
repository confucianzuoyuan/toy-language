package cw1

abstract class Rexp

case object ZERO extends Rexp

case object ONE extends Rexp

case object ALL extends Rexp

case class CHAR(c: Char) extends Rexp

case class ALT(r1: Rexp, r2: Rexp) extends Rexp

case class SEQ(r1: Rexp, r2: Rexp) extends Rexp

case class STAR(r: Rexp) extends Rexp

case class RANGE(cs: Set[Char]) extends Rexp // set of characters

case class PLUS(r: Rexp) extends Rexp // plus

case class OPTIONAL(r: Rexp) extends Rexp // optional

case class INTER(r1: Rexp, r2: Rexp) extends Rexp // intersection

case class NTIMES(r: Rexp, n: Int) extends Rexp // n-times

case class UPTO(r: Rexp, n: Int) extends Rexp // up n-times

case class FROM(r: Rexp, n: Int) extends Rexp // from n-times

case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp // between nm-times

case class NOT(r: Rexp) extends Rexp // not

case class CFUN(f: Char => Boolean) extends Rexp

val allCharacterSet = ('a' to 'z').toSet ++ ('0' to '9').toSet ++ Set('_', '.', '-')


// the nullable function: tests whether the regular
// expression can recognise the empty string
def nullable(r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case ALL => false
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case RANGE(_) => false
  case PLUS(r) => nullable(r)
  case OPTIONAL(_) => true
  case INTER(r1, r2) => nullable(r1) && nullable(r2)
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case UPTO(r, n) => true
  case FROM(r, n) => n == 0 || nullable(r)
  case BETWEEN(r, n, m) => n == 0 || nullable(r)
  case NOT(r) => !nullable(r)
  case CFUN(f) => false
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp): Rexp =
  r match {
    case ZERO => ZERO
    case ONE => ZERO
    case ALL => der(c, CFUN(x => allCharacterSet.contains(x)))
    case CHAR(d) => der(c, CFUN(x => x == d))
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) =>
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r1) => SEQ(der(c, r1), STAR(r1))
    case RANGE(cs) => der(c, CFUN(x => cs.contains(x)))
    case PLUS(r) => SEQ(der(c, r), STAR(r))
    case OPTIONAL(r) => ALT(der(c, r), ZERO)
    case INTER(r1, r2) => SEQ(der(c, r1), der(c, r2))
    case NTIMES(r, i) =>
      if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
    case UPTO(r, n) =>
      if (n == 0) ZERO else SEQ(der(c, r), UPTO(r, n - 1))
    case FROM(r, n) =>
      if (n == 0) SEQ(der(c, r), STAR(r)) else SEQ(der(c, r), FROM(r, n - 1))
    case BETWEEN(r, n, m) =>
      var r1: Rexp = NTIMES(r, n)
      for
        i <- (n + 1) to m
      do
        r1 = ALT(r1, NTIMES(r, i))
      der(c, r1)
    case NOT(r) => NOT(der(c, r))
    case CFUN(f) => if (f(c)) ONE else ZERO
  }

// simplification
def simp(r: Rexp): Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT(r1s, r2s)
  }
  case SEQ(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

// the derivative w.r.t. a string (iterates der and simp)
def ders(s: List[Char], r: Rexp): Rexp = s match {
  case Nil => r
  case c :: s => ders(s, simp(der(c, r)))
}

// the main matcher function
def matcher(r: Rexp, s: String): Boolean =
  nullable(ders(s.toList, r))


@main
def main(): Unit =
  println(allCharacterSet)
  // question-1
  // corner case
  assert(matcher(NTIMES(CHAR('a'), 0), ""))
  // 1st-column
  assert(matcher(OPTIONAL(CHAR('a')), ""))
  assert(matcher(OPTIONAL(CHAR('a')), "a"))
  assert(!matcher(OPTIONAL(CHAR('a')), "aa"))
  assert(!matcher(OPTIONAL(CHAR('a')), "aaa"))
  assert(!matcher(OPTIONAL(CHAR('a')), "aaaaa"))
  assert(!matcher(OPTIONAL(CHAR('a')), "aaaaaa"))
  // 2nd-column
  assert(matcher(NOT(CHAR('a')), ""))
  assert(!matcher(NOT(CHAR('a')), "a"))
  assert(matcher(NOT(CHAR('a')), "aa"))
  assert(matcher(NOT(CHAR('a')), "aaa"))
  assert(matcher(NOT(CHAR('a')), "aaaaa"))
  assert(matcher(NOT(CHAR('a')), "aaaaaa"))
  // 3rd-column
  assert(!matcher(NTIMES(CHAR('a'), 3), ""))
  assert(!matcher(NTIMES(CHAR('a'), 3), "a"))
  assert(!matcher(NTIMES(CHAR('a'), 3), "aa"))
  assert(matcher(NTIMES(CHAR('a'), 3), "aaa"))
  assert(!matcher(NTIMES(CHAR('a'), 3), "aaaaa"))
  assert(!matcher(NTIMES(CHAR('a'), 3), "aaaaaa"))
  // 4th-column
  assert(matcher(NTIMES(OPTIONAL(CHAR('a')), 3), ""))
  assert(matcher(NTIMES(OPTIONAL(CHAR('a')), 3), "a"))
  assert(matcher(NTIMES(OPTIONAL(CHAR('a')), 3), "aa"))
  assert(matcher(NTIMES(OPTIONAL(CHAR('a')), 3), "aaa"))
  assert(!matcher(NTIMES(OPTIONAL(CHAR('a')), 3), "aaaaa"))
  assert(!matcher(NTIMES(OPTIONAL(CHAR('a')), 3), "aaaaaa"))
  // 5th-column
  assert(matcher(UPTO(CHAR('a'), 3), ""))
  assert(matcher(UPTO(CHAR('a'), 3), "a"))
  assert(matcher(UPTO(CHAR('a'), 3), "aa"))
  assert(matcher(UPTO(CHAR('a'), 3), "aaa"))
  assert(!matcher(UPTO(CHAR('a'), 3), "aaaaa"))
  assert(!matcher(UPTO(CHAR('a'), 3), "aaaaaa"))
  // 6th-column
  assert(matcher(UPTO(OPTIONAL(CHAR('a')), 3), ""))
  assert(matcher(UPTO(OPTIONAL(CHAR('a')), 3), "a"))
  assert(matcher(UPTO(OPTIONAL(CHAR('a')), 3), "aa"))
  assert(matcher(UPTO(OPTIONAL(CHAR('a')), 3), "aaa"))
  assert(!matcher(UPTO(OPTIONAL(CHAR('a')), 3), "aaaaa"))
  assert(!matcher(UPTO(OPTIONAL(CHAR('a')), 3), "aaaaaa"))
  // 7th-column
  assert(!matcher(BETWEEN(CHAR('a'), 3, 5), ""))
  assert(!matcher(BETWEEN(CHAR('a'), 3, 5), "a"))
  assert(!matcher(BETWEEN(CHAR('a'), 3, 5), "aa"))
  assert(matcher(BETWEEN(CHAR('a'), 3, 5), "aaa"))
  assert(matcher(BETWEEN(CHAR('a'), 3, 5), "aaaaa"))
  assert(!matcher(BETWEEN(CHAR('a'), 3, 5), "aaaaaa"))
  // 8th-column
  assert(matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), ""))
  assert(matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), "a"))
  assert(matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), "aa"))
  assert(matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), "aaa"))
  assert(matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), "aaaaa"))
  assert(!matcher(BETWEEN(OPTIONAL(CHAR('a')), 3, 5), "aaaaaa"))

  // question-2

  // question-3
  val emailRegex = SEQ(PLUS(ALL), SEQ(CHAR('@'), SEQ(PLUS(RANGE(('a' to 'z').toSet ++ ('0' to '9').toSet ++ Set('.', '-'))), SEQ(CHAR('.'), BETWEEN(RANGE(('a' to 'z').toSet ++ Set('.')), 2, 6)))))
  assert(matcher(emailRegex, "zuoyuantc@126.com"))

  // question-4
  val r1 = SEQ(STAR(ALL), SEQ(CHAR('*'), SEQ(CHAR('/'), STAR(ALL))))
  val r2 = NOT(r1)
  val r3 = SEQ(CHAR('/'), SEQ(CHAR('*'), r2))
  val r = SEQ(CHAR('/'), SEQ(CHAR('*'), SEQ(r2, SEQ(CHAR('*'), CHAR('/')))))
  assert(matcher(r, "/**/"))
  assert(matcher(r, "/*foobar*/"))
  assert(!matcher(r, "/*test*/test*/"))
  assert(matcher(r, "/*test/*test*/"))

  // question-5
  val r51 = SEQ(CHAR('a'), SEQ(CHAR('a'), CHAR('a')))
  val r52 = SEQ(BETWEEN(CHAR('a'), 19, 19), OPTIONAL(CHAR('a')))
  val r51_ = PLUS(PLUS(r51))
  val r52_ = PLUS(PLUS(r52))
  val s1 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  assert(s1.length == 120)
  val s2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  assert(s2.length == 131)
  val s3 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  assert(s3.length == 136)
  println(matcher(r51_, s1))
  println(matcher(r52_, s1))
  println(matcher(r51_, s2))
  println(matcher(r52_, s2))
  println(matcher(r51_, s3))
  println(matcher(r52_, s3))