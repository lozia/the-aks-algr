/**
 * This is an implementation of the Sieve of Eratosthenes. You may find many similar contents on the internet, because this is how the Sieve works.
 * Fill free to distribute the work, because I do not own this method.
 * @author Zongyao Li on 11/19/2021
 * @version 1.0.0.
 */

object TheSieve {
  /**
   * This should not be used outside of this object. This creates a table for the Sieve, which starts at 2.
   * @return a LazyList starting at 2.
   */
  val starter:LazyList[Int] = LazyList.from(2)

  /**
   * This is the process of working out the Sieve algo. It uses recursion takes the tail of current list and filters out
   * the the first element of the tail if it is a composite number of previous heads.
   * @param table the table where we want to cross out composites.
   * @return a new table that has composites crossed out.
   */
  def filter_out_composites(table: LazyList[Int]): LazyList[Int] = {
    table.head #:: filter_out_composites(table.tail filter(n => n % table.head != 0))
  }

  /**
   * This function should be the actual one to use if requires a final table with [primes < n] in it.
   * @param n the number that the table ends at.
   * @return a LazyList[Int] which performs as the table.
   */
  def get_n_primes(n:Int): LazyList[Int] = filter_out_composites(starter) take n

  /**
   * This function should be the actual one to use if requires a final table with (virtually) infinite primes in it.
   * @return a LazyList[Int] which performs as the table.
   */
  def get_primes: LazyList[Int] = filter_out_composites(starter)
}

object run_the_sieve extends App{
  val inf_primes = TheSieve.get_primes
  val n_primes = TheSieve.get_n_primes(30)
  println("Infinite prime table works: ")
  inf_primes.take(30) map(n => n.toString + " ") foreach(print(_))
  println("\nFixed size prime table works: ")
  n_primes.map(n => n.toString + " ") foreach( print(_))
}