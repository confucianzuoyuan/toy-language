// Rexps
abstract class Rexp

case object ZERO extends Rexp

case object ONE extends Rexp

case class CHAR(c: Char) extends Rexp

case class ALT(r1: Rexp, r2: Rexp) extends Rexp

case class SEQ(r1: Rexp, r2: Rexp) extends Rexp

case class STAR(r: Rexp) extends Rexp

// new regular expressions
case class RANGE(s: Set[Char]) extends Rexp

case class PLUS(r: Rexp) extends Rexp

case class OPTIONAL(r: Rexp) extends Rexp

case class NTIMES(r: Rexp, n: Int) extends Rexp

case class RECD(x: String, r: Rexp) extends Rexp

// values - you might have to extend them
// according to which values you want to create
// for the new regular expressions
abstract class Val

case object Empty extends Val

case class Chr(c: Char) extends Val

case class Sequ(v1: Val, v2: Val) extends Val

case class Left(v: Val) extends Val

case class Right(v: Val) extends Val

case class Stars(vs: List[Val]) extends Val

case class Rec(x: String, v: Val) extends Val


// convenience for typing regular expressions

import scala.language.implicitConversions

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c :: Nil => CHAR(c)
  case c :: s => SEQ(CHAR(c), charlist2rexp(s))
}

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def ~(s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def |(s: Rexp) = ALT(r, s)
}

extension (s: String) {
  def $(r: Rexp) = RECD(s, r)
  def |(r: Rexp) = ALT(s, r)
  def |(r: String) = ALT(s, r)
  def % = STAR(s)
  def ~(r: Rexp) = SEQ(s, r)
  def ~(r: String) = SEQ(s, r)
}


// nullable (needs to be extended for new regular expressions)
def nullable(r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case OPTIONAL(_) => true
  case PLUS(r) => nullable(r)
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case RANGE(_) => false
  case RECD(_, r) => nullable(r)
}

// der (needs to be extended for new regular expressions)
def der(c: Char, r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r) => ALT(der(c, r), ZERO)
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
  case RECD(x, r) => der(c, r)
}

// flatten (needs to work with all values)
def flatten(v: Val): String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(x, v) => flatten(v)
}

// env (needs to work with all values)
def env(v: Val): List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flatten(v)) :: env(v)
}

// mkeps (needs to be extended for new regular expressions)
def mkeps(r: Rexp): Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case PLUS(r) => mkeps(SEQ(r, STAR(r)))
  case OPTIONAL(r) => mkeps(ALT(r, ZERO))
  case NTIMES(r, n) =>
    var l: List[Val] = List()
    for (i <- 1 to n) {
      l = l :+ mkeps(r)
    }
    Stars(l)
  case RECD(x, r) => Rec(x, mkeps(r))
}

// inj (needs to be extended for new regular expressions)
def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
  case (PLUS(r), v) => inj(SEQ(r, STAR(r)), c, v)
  case (NTIMES(r, n), Sequ(v1, Stars(v2))) => Stars(List(inj(r, c, v1)) ++ v2)
  case (OPTIONAL(r), v) => inj(ALT(r, ZERO), c, v)
  case (RANGE(cs), Empty) => Chr(c)
  case (RECD(l, r), v) =>
    Rec(l, inj(r, c, v))
}


// the simplification and rectification part
// can be left untouched

// rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v: Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v: Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v: Val) => v match {
  case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

// simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
      else (ALT(r1s, r2s), F_ALT(f1s, f2s))
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s, r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID) // this part handles all new regular expressions
}

// lexing generating a value
def lex_simp(r: Rexp, s: List[Char]): Val = s match {
  case Nil =>
    if (nullable(r))
      mkeps(r)
    else {
      throw new Exception("lexing error")
    }
  case c :: cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

// lexing extracting a list of String-String pairs
def lexing_simp(r: Rexp, s: String): List[(String, String)] =
  env(lex_simp(r, s.toList))


// Language specific code for the While Language
// (you need to create the regular expressions - see CW2)

val KEYWORD: Rexp = "if" | "then" | "true" | "false" | "else" | "do" | "while" | "for" | "to" | "read" | "write" | "skip"
val OP: Rexp = ">" | ">=" | "<" | "<=" | "!=" | "+" | "%" | "/" | "-" | "*" | "==" | ":=" | "||" | "&&"
val LET: Rexp = RANGE(('a' to 'z').toSet) | RANGE(('A' to 'Z').toSet)
val SYM: Rexp = LET | "." | "_" | ">" | "<" | "=" | ";" | "," | ":" | "\\"
val PARENS: Rexp = "{" | "}" | "(" | ")"
val DIGIT: Rexp = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
val SEMI: Rexp = ";"
val WHITESPACE: Rexp = " " | "\n" | "\r" | "\t"
val ID: Rexp = LET ~ %("_" | LET | DIGIT)
val NUMBERS: Rexp = "0" | (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ~ %(DIGIT))
val STRING: Rexp = "\"" ~ %(SYM | DIGIT | PARENS | WHITESPACE) ~ "\""
val EOL: Rexp = "\n" | "\r\n"
val COMMENT: Rexp = "//" ~ %(SYM | LET | " " | PARENS | DIGIT | OP | STRING) ~ EOL

val WHILE_REGS = (("k" $ KEYWORD) |
  ("o" $ OP) |
  ("str" $ STRING) |
  ("p" $ PARENS) |
  ("s" $ SEMI) |
  ("w" $ WHITESPACE) |
  ("i" $ ID) |
  ("n" $ NUMBERS) |
  ("c" $ COMMENT)).%

// Token
abstract class Token extends Serializable

case class T_KEYWORD(s: String) extends Token

case class T_OP(s: String) extends Token

case class T_STRING(s: String) extends Token

case class T_PAREN(s: String) extends Token

case object T_SEMI extends Token

case class T_ID(s: String) extends Token

case class T_NUM(n: Int) extends Token

val token: PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KEYWORD(s)
  case ("o", s) => T_OP(s)
  case ("str", s) => T_STRING(s)
  case ("p", s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  case ("i", s) => T_ID(s)
  case ("n", s) => T_NUM(s.toInt)
}

// Tokenise
def tokenise(s: String): List[Token] =
  lexing_simp(WHILE_REGS, s).collect(token)

// CW3
//=====
//
// The main idea is to extend the parser form the lectures
// (which processes strings) to a parser that processes
// tokens. For this you need to use the lexer from CW2 and
// possibly adjust the lexing regular expressions accordingly.


// IMPORTANT:
//
// you need to include the lexer from CW2, which defines
// Tokens and tokenise
//
// one way to do it is via the import statements
//
//import $file.^.cw2.cw02
//import cw02._
//
// or copy the code into this directory / file
//


// parser combinators

type IsSeq[I] = I => Seq[?]

abstract class Parser[I, T](using is: IsSeq[I])  {
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in);
         if is(tl).isEmpty) yield hd
}

// parser combinators

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T],
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)
}

// sequence parser
class SeqParser[I: IsSeq, T, S](p: => Parser[I, T],
                                q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(in: I) =
    for ((hd1, tl1) <- p.parse(in);
         (hd2, tl2) <- q.parse(tl1)) yield ((hd1, hd2), tl2)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T],
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}


// some convenient syntax for parser combinators
extension [I: IsSeq, T](p: Parser[I, T]) {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

// Abstract Syntax Trees
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteId(s: String) extends Stmt  // for printing values of variables
case class WriteString(s: String) extends Stmt  // for printing words

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class Lop(o: String, b1: BExp, b2: BExp) extends BExp

case class p(s: String) extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_OP(_s) :: rest if s == _s  => Set((s, rest))
    case T_PAREN(_s) :: rest if s == _s  => Set((s, rest))
    case T_KEYWORD(_s) :: rest if s == _s => Set((s, rest))
    case T_SEMI :: rest if s == ";" => Set((";", rest))
    case _ => Set()
  }
}

case object StringParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_STRING(s) :: rest => Set((s, rest))
    case _ => Set()
  }
}

case object VarParser extends Parser[List[Token], AExp] {
  def parse(ts: List[Token]) = ts match {
    case T_ID(s) :: rest => Set((Var(s), rest))
    case _ => Set()
  }
}

case object NumParser extends Parser[List[Token], AExp] {
  def parse(ts: List[Token]) = ts match {
    case T_NUM(n) :: rest => Set((Num(n), rest))
    case _ => Set()
  }
}

case object BoolParser extends Parser[List[Token], BExp] {
  def parse(ts: List[Token]) = ts match {
    case T_KEYWORD("true") :: rest => Set((True, rest))
    case T_KEYWORD("false") :: rest => Set((False, rest))
    case _ => Set()
  }
}

lazy val E: Parser[List[Token], AExp] =
  (T ~ p("+") ~ E).map{ case ((x, y), z) => Aop("+", x, z)}
|| (T ~ p("-") ~ E).map{ case ((x, y), z) => Aop("-", x, z)}
|| T

lazy val T: Parser[List[Token], AExp] =
  (F ~ p("*") ~ T).map{ case ((x, y), z) => Aop("*", x, z)}
|| (F ~ p("/") ~ T).map{ case ((x, y), z) => Aop("/", x, z)}
|| (F ~ p("%") ~ T).map{ case ((x, y), z) => Aop("%", x, z)}
|| F

lazy val F: Parser[List[Token], AExp] =
  (p("(") ~ E ~ p(")")).map{ case ((x, y), z) => y} || NumParser || VarParser

lazy val C: Parser[List[Token], BExp] =
  (E ~ p(">") ~ E).map{ case ((x, y), z) => Bop(">", x, z)}
|| (E ~ p(">=") ~ E).map{ case ((x, y), z) => Bop(">=", x, z)}
|| (E ~ p("<") ~ E).map{ case ((x, y), z) => Bop("<", x, z)}
|| (E ~ p("<=") ~ E).map{ case ((x, y), z) => Bop("<=", x, z)}
|| (E ~ p("!=") ~ E).map{ case ((x, y), z) => Bop("!=", x, z)}
|| (E ~ p("==") ~ E).map{ case ((x, y), z) => Bop("==", x, z)}
|| (p("(") ~ A ~ p(")")).map{ case ((x, y), z) => y}
|| BoolParser

lazy val B: Parser[List[Token], BExp] =
  (C ~ p("&&") ~ C).map{ case ((x, y), z) => Lop("&&", x, z)}
|| C

lazy val A: Parser[List[Token], BExp] =
  (B ~ p("||") ~ B).map{ case ((x, y), z) => Lop("||", x, z)}
|| B

// Parser rules
lazy val AExp: Parser[List[Token], AExp] = E

lazy val BExp: Parser[List[Token], BExp] = A

lazy val Stmt: Parser[List[Token], Stmt] =
  (p("write") ~ StringParser).map{ case (x, y) => WriteString(y)}
|| (p("write") ~ p("(") ~ StringParser ~ p(")")).map { case (((x, y), z), u) => WriteString(z)}
|| (p("write") ~ VarParser).map{ case (x, Var(y)) => WriteId(y)}
|| (p("write") ~ p("(") ~ VarParser ~ p(")")).map { case (((x, y), Var(z)), u) => WriteId(z)}
|| (p("read") ~ VarParser).map{ case (x, Var(y)) => Read(y)}
|| (VarParser ~ p(":=") ~ AExp).map{ case ((Var(x), y), z) => Assign(x, z)}
|| p("skip").map{ x => Skip}
|| (p("while") ~ BExp ~ p("do") ~ Block).map{ case (((x, y), z), w) => While(y, w)}
|| (p("if") ~ BExp ~ p("then") ~ (Stmts || Block) ~ p("else") ~ (Stmts || Block)).map{ case (((((u, v), w), x), y), z) => If(v, x, z)}

lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ p(";") ~ Stmts).map{ case ((x, y), z) => x :: z }
|| (Stmt ~ p(";")).map{ case (x, y) => List(x)}
|| (Stmt ~ Stmts).map{ case (x, y) => x :: y}
|| Stmt.map{x => List(x)}

lazy val Block: Parser[List[Token], Block] =
  (p("{") ~ Stmts ~ p("}")).map{ case ((x, y), z) => y}


// Interpreter
//=============

// Import needed to take an int as input from the user
import scala.io.StdIn.readInt

type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Var(var_name) => env.get(var_name) match {
    case Some(i) => i
    case None => throw new RuntimeException("no var in env.")
  }
  case Num(i) => i
  case Aop(o, a1, a2) => o match {
    case "+" => eval_aexp(a1, env) + eval_aexp(a2, env)
    case "-" => eval_aexp(a1, env) - eval_aexp(a2, env)
    case "*" => eval_aexp(a1, env) * eval_aexp(a2, env)
    case "/" => eval_aexp(a1, env) / eval_aexp(a2, env)
    case "%" => eval_aexp(a1, env) % eval_aexp(a2, env)
  }
}
def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop(o, a1, a2) => o match {
    case ">" => eval_aexp(a1, env) > eval_aexp(a2, env)
    case ">=" => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case "<" => eval_aexp(a1, env) < eval_aexp(a2, env)
    case "<=" => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case "==" => eval_aexp(a1, env) == eval_aexp(a2, env)
    case "!=" => eval_aexp(a1, env) != eval_aexp(a2, env)
  }
  case Lop(o, b1, b2) => o match {
    case "&&" => eval_bexp(b1, env) && eval_bexp(b2, env)
    case "||" => eval_bexp(b1, env) || eval_bexp(b2, env)
  }
}
def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case WriteString(s) =>
    print(s.substring(1, s.length - 1).replace("\\n", "\n"))
    env
  case WriteId(var_name) => env.get(var_name) match {
    case Some(i) => print(i); env
    case None => throw new RuntimeException("no var in env: " + var_name)
  }
  case Read(var_name) =>
    val input = readInt()
    env + (var_name -> input)
  case Assign(s, a) => env + (s -> eval_aexp(a, env))
  case If(a, bl1, bl2) =>
    if (eval_bexp(a, env)) eval_bl(bl1, env)
    else eval_bl(bl2, env)
  case While(b, bl) =>
    var e = env
    while (eval_bexp(b, e)) {
      e = eval_bl(bl, e)
    }
    e
}

def eval_bl(bl: Block, env: Env): Env = {
  var e = env
  for (b <- bl) {
    e = eval_stmt(b, e)
  }
  e
}

// The main eval function for the interpreter
def eval(bl: Block): Env = eval_bl(bl, Map())

// Function for testing individual files
def single_test(file: String): Unit = {
  val contents = os.read(os.pwd / "examples" / file)
  println(s"Lex $file: ")
  val tk = tokenise(contents)
  println(tk.mkString(","))
  println(s"Parse $file: ")
  val ast = Stmts.parse_all(tk).head
  println(ast)
  println(s"Eval $file: ")
  println(eval(ast))
}
@main
// Test all example files
def test(): Unit = {
  val examples = List("collatz.while", "collatz2.while", "factors.while", "fib.while", "loops.while", "primes.while")
  examples.foreach { file =>
    single_test(file)
  }
}
