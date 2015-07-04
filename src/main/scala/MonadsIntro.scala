/**
 * Lets start by defining containers. Container is a class parameterized by type.
 * Examples include Lists, Option types and so on.
 *
 * The next definition is monad. Monad is a container with two operations defined.
 *
 * The first operation takes a value (of type by which container is parameterized)
 * and wraps this value into container. This operation is called unit. In scala we
 * seldom define this operation explicitly.
 *
 * The second operation takes a container, unwraps the value(s) and gives this value(s)
 * to an user supplied function. This operation is called flatMap in scala and is the
 * essence of a monad. The main thing differentiating monads is the difinition of
 * flatMap function.
 *
 * In scala, if we define fmap and map methods for a given monadic container, then
 * a nicer syntax using for comprehensions is available. The good news is that
 * we can define flatMap, map, filter operations on our custom types and have for
 * comprehension syntax for these types
 */
object MonadsIntro {
  def main(args: Array[String]): Unit = {
    /**
     * List monad extracts elements from list and applies a given funstion to eeach
     * extracted element. This function produces list for every extracted element. Final
     * result is calculated by concatenating all lists.
     */
    val list1 = List(1, 2, 3)
    val list2 = List(10, 20, 30)

    // The below given example may be quite unintuitive at first
    val sumsOfElements1 = list1.flatMap { element1 =>
      list2.map{element2 => element1+element2}
    }
    println(sumsOfElements1) //List(11, 21, 31, 12, 22, 32, 13, 23, 33)

    //The same calculation using for comprehensions looks much more familiar
    val sumsOfElements2 = for {
      element1 <- list1
      element2 <- list2
    } yield (element1 + element2) //List(11, 21, 31, 12, 22, 32, 13, 23, 33)
    println(sumsOfElements2)

    //Google can give a lot of examples of monad usages for Options, Futures, Sql queries and so
    //on. In order to show the diversity of monad application, let us take a look at parsing
  }
}