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
   * filter function should be defined, we will also use it later in for comprehensions.
   * Any if statement in for comprehension uses this function. It is possible to live without
   * this functions but sometimes it is just very convenient (take a look at "satisfy" function)
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

  /**
   * Choice operator is defined below. It first tries the first parser. If this succeeds then result is
   * returned. If first parser fails then the second parser is tried and the result of the second parse
   * is returned.
   * This is low level combinator and cannot be implemented using already defined combinators.
   * In general, all low level parsers must be implemented in a library and implementation of higher level
   * parsers may be left to an user (for example "digit" could have been left of the library, though
   * it is convenient)
   * B >: A can be oveided if Parser class would be declared without +A. In general it is preferable to
   * declare type parameters as covariant if this is possible
   * The operator must be declared here (and not in companion object) if we want to use operator notation
   * like "parser1 <|> parser2"
   */
  def <|>[B >: A](p: Parser[B]): Parser[B] = Parser { str =>
    this.parse(str) match {
      case ok1@Some(_) => ok1
      case None =>
        p.parse(str) match {
          case ok2@Some(_) => ok2
          case None => None
        }
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
   * Combining these functions we will build our parsers.
   */

  /**
   * Parser that matches any char if input is not empty. Thar char is returned as a result
   */
  def anyChar: Parser[Char] = Parser { str =>
    if (str == "") None
    else Some(str(0), str.tail)
  }

  /**
   * anyChar parser matches any char unconditionally. For more complex logical decisions, conditional
   * matching is required. Such conditional matching is done by our satisfy parser. If current character
   * satisfies the supplied predicate, then that character is returned. Else parse is unsuccessful.
   * Notice here we already use the defined monadic operatios (with for comprehensions sugar)
   */
  def satisty(predicate: Char => Boolean): Parser[Char] = for {
    c <- anyChar
    if (predicate(c))
  } yield c

  /**
   * Parser which returns char if the char is digit. Otherwise fails.
   * Here we are already using combinators. It becomes powerful :)
   */
  def digit: Parser[Char] = satisty(_.isDigit)

  /**
   * The following two combinators are almost always defined together (as in the
   * most straightforward implementation they depend on each other). They match
   * a given parser zero or more times (one or more times). The result from parsers
   * is returned collected in a sequence (Seq is very similar interface as List in Java)
   */
  def many[A](p: Parser[A]): Parser[Seq[A]] = many1(p) <|> unit(Seq.empty)

  def many1[A](p: Parser[A]): Parser[Seq[A]] = for {
    res <- p
    resSeq <- many(p)
  } yield (res +: resSeq)

  /**
   * And for some more helper parsers...
   * Names must and return types should be self explanatory
   */
  def integer: Parser[Double] = for { //returns double, though input is integer
    r <- many1(digit)
  } yield r.mkString("").toDouble

  def floating: Parser[Double] = for {
    integerPart <- many1(digit)
    point <- satisty(_ == '.')
    floatingPart <- many1(digit)
  } yield ((integerPart :+ point) ++ floatingPart).mkString("").toDouble

  def skipWhiteSpace: Parser[Unit] = for {
    _ <- many(satisty(_ == ' '))
  } yield Unit

  /**
   * This parser lifts a given parser to white space insensitive parser
   * This may be useful when tokenizing for example, as white space is
   * insignificant in many languages
   */
  def liftToWhiteSpaceInsensitive[A](p: Parser[A]): Parser[A] = for {
    _ <- skipWhiteSpace
    res <- p
    _ <- skipWhiteSpace
  } yield res

}
