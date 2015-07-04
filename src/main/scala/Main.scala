/**
 * Parser testing
 */
object Main {
  def main( args: Array[String]): Unit = {
    val strToParse = "sasdasd"
    val parseResult = Parser.anyChar.parse(strToParse)
    println(parseResult)

    val pp: Parser[String] = for {
      p1 <- Parser.anyChar
      p2 <- Parser.anyChar
    } yield (p1.toString + p2.toString)

    val parseResult1 = pp.parse(strToParse)
    println(parseResult1)
  }



}
