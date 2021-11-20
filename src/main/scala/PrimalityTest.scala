import scala.io.StdIn._


case class TestNumber(n: BigInt){
  val getN: BigInt = n
}

object PrimalityTest extends App {
  println("Please enter the number to be tested: ")
  val big_number = BigInt(readLine())
  // println(TheSieve.get_primes(100))
}
