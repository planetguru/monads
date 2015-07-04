/**
 * Below given text is heavily based (and heavily modified) on parser combinators
 * description given in
 * http://www.cs.uwyo.edu/~jlc/courses/3015/parser_pearl.pdf
 * I will try to adapt the main ideas to scala. The main task is still the same as
 * it was in the beginning - to show off monads.
 *
 * Parsing is usually done using one of two methods - parser generators or parser
 * combinators. Combinators are written in a host langugage, that's their main advantage.
 * It is easier to extend and understand them, but they usually are not as performant
 * as generators.
 */

/**
 * Parser is a function that takes a string as argument and returns a result and remaining
 * string as a pair. This pair is wrapped in an Option as a parse may fail.
 * So, in case of a successful parse, a value and a remaining string are returned inside Some
 * constructor
 * In case parsing is unsuccessful, None is returned
 */
final case class Parser[+A](parse: String => Option[(A, String)]) {
  /**
   * The main monadic function - flatMap - will be implemented below
   * we should interpret flatMap as two parsers concatenated (two parsings performed
   * in sequence) and the second parsing may depend on the result of the first one.
   * Implementation may seem complex at first (probably it is objectively somewhat complex)...
   * but follow the types... the first argument to flatMap is implicit "this"
   * the second is function which takes unwrapped value from monad and produces a new monad
   * (in this case our monad is parser).
   * Also notice that type parameter in second parser may change (this applies to all monads -
   * function to flatMap may change type parameter)
   */
  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { str =>  //function is wrapped in "Parser" constructor
    val resultFromFirstParse: Option[(A, String)] = parse(str) //at first parse input using "this" parser
    resultFromFirstParse match {
      case None => None
      case Some((value, remainingString)) =>  //type annotation needed
        val secondParser = f(value)  //now apply function f to value to get a second parser
        val resultFromSecondParse = secondParser.parse(remainingString) //now apply the second parser to string
        resultFromSecondParse   //finally, return the result
    }
  }

  /**
   * We also need map function in order for comprehension syntax to work
   */
  def map[B](f: A => B): Parser[B] = Parser {str =>
    parse(str) match {
      case None => None
      case Some((value, remainingStr)) => Some((f(value), remainingStr))
    }
  }

  /**
   * To formally satisfy interface needed for for comprehensions
   * filter function should be defined, though in reality it is not needed at all
   */
  def filter(p: A => Boolean): Parser[A] = Parser {str =>
    val resultFromParse = parse(str)
    resultFromParse match {
      case None => None
      case Some((value, remainingString)) =>
        if (p(value)) Some(value, remainingString)
        else None
    }
  }
}

/**
 * Companion object for helper functions, unit etc
 */
object Parser {
  /**
   * Turning back tu monads. This time we will also define unit function
   * for our parser monad. Unit function for this monad is extremely simple
   * (as is almost is with unit functions for monads) - it does not touch the input
   * string, and returns the given value - all wrapped in Some
   */
  def unit[A](value: A) = Parser(str => Some(value, str))

  /**
   * Below are combinators - functions used for parser construction.
   * Combining these functions we will build our parsers
   */

  /**
   * Parser that matches any char if input is not empty. Thar char is returned as a result
   */
  def anyChar: Parser[Char] = Parser { str =>
    if (str == "") None
    else Some(str(0), str.tail)
  }
}
