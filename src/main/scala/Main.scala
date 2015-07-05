/**
 * Parser testing
 */
object Main {
  def main( args: Array[String]): Unit = {
    // CHANGE THIS IN ORDER TO OBSERVE OUTCOME
    //it can be seen that the parser is quite robust
    val strToParse = " ( 2*22.23+  (1+2) *3 + 5 )* 3 "
    ArithmeticParser.evalAll(strToParse) //expression  ( 2*22.23+  (1+2) *3 + 5 )* 3  was evaluated. Result is 175.38
  }

}

/**
 * Below we will construct a parser for arithmetic expression parsing using real numbers
 * Parsing in itself gives us no interesting results, what we are interested in is calculation
 * of expression result (value of evaluated expression). So we will define a class hierarchy for
 * arithmetic expressions interpretation and a parser which returns an expression. It is a very
 * standard method
 */

/**
 * class for interpretation of expressions. Using these classes AST for expressions will be built
 * Pattern matching is used heavily
 */

abstract class Expression {
  def eval: Double = this match {
    case Number(a) => a
    case Plus(e1, e2) => e1.eval + e2.eval
    case Minus(e1, e2) => e1.eval - e2.eval
    case Mult(e1, e2) => e1.eval * e2.eval
    case Div(e1, e2) => e1.eval / e2.eval  //clearly DivideByZero exception could be thrown (we will neglect this here)
  }
}
case class Number(number: Double) extends Expression
case class Plus(e1: Expression, e2: Expression) extends Expression
case class Minus(e1: Expression, e2: Expression) extends Expression
case class Mult(e1: Expression, e2: Expression) extends Expression
case class Div(e1: Expression, e2: Expression) extends Expression

object ArithmeticParser {
  import Parser._  //importing all names from parser object, this way no "Parser" prefix is needed

  /**
   * Main function which produces results or fails.
   * There is no any error reporting in the parser
   * so fails are unpleasant, but its just a toy demonstration, so...
   */
  def evalAll(s: String): Unit = {
    additive.parse(s) match {
      case None =>
        println("Parsing failed")
      case Some((expr, "")) =>
        println(s"expression ${s} was evaluated. Result is ${expr.eval}")
      case Some(_) =>
        println("Not all input consumed")
    }
  }

  /**
   * This parser parses floating point numbers in usual "point notation" (my own term, probably incorrect)
   * We are returning this number wrapped inside Number class.
   * One advantage of parser combinators is that no regular expressions are in sight. This also may
   * be disadvantage if enormous performance is required
   */
  def number: Parser[Expression] = for {
    //notice that floating must go first. Otherwise integer will succeed and a lot of
    //input will be left unconsumed
    res <- liftToWhiteSpaceInsensitive(floating <|> integer)
  } yield Number(res)

  /**
   * EBNF like grammar for our expression parser is given as
   *
   * Additive ← Multitive ‘+’ Additive | Multitive
   * Multitive ← Primary ‘*’ Multitive | Primary
   * Primary ← ‘(’ Additive ‘)’ | Number
   * Number ← FloatingPointNumber
   *
   * our parsing functions correspond to this very closely
   * Parser for numbers we already have
   */
  def additive: Parser[Expression] = {
    val left = for {
      res1 <- multitive
      _ <- liftToWhiteSpaceInsensitive(satisty(_ == '+'))
      res2 <- additive
    } yield Plus(res1, res2)
    left <|> multitive
  }

  def multitive: Parser[Expression] = {
    val left = for {
      res1 <- primary
      _ <- liftToWhiteSpaceInsensitive(satisty(_ == '*'))
      res2 <- multitive
    } yield Mult(res1, res2)
    left <|> primary
  }

  def primary: Parser[Expression] = {
    val left = for {
      _ <- liftToWhiteSpaceInsensitive(satisty(_ == '('))
      res1 <- additive
      _ <- liftToWhiteSpaceInsensitive(satisty(_ == ')'))
    } yield res1
    left <|> number
  }
}