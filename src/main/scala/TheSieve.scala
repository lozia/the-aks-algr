import java.io.{BufferedWriter, File, FileWriter}

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
  val starter:LazyList[Long] = LazyList.from(2).map(_.toLong)

  /**
   * This is the process of working out the Sieve algo. It uses recursion that takes the tail of current list and filters out
   * the elements of the tail if they are composite number of previous heads.
   * @param table the table where we want to cross out composites.
   * @return a new table that has composites crossed out.
   */
  def filter_out_composites(table: LazyList[Long]): LazyList[Long] = {
    table.head #:: filter_out_composites(table.tail filter(n => n % table.head != 0))
  }

  /**
   * This function should be the actual one to use if requires a final table with [primes < n] in it.
   * @param n the number that the table ends at.
   * @return a LazyList[Int] which performs as the table.
   */
  def get_n_primes(n:Int): LazyList[Long] = filter_out_composites(starter) take n

  /**
   * This function should be the actual one to use if requires a final table with (virtually) infinite primes in it.
   * @return a LazyList[Int] which performs as the table.
   */
  val primes: LazyList[Long] = 2L #:: LazyList.from(3).map(_.toLong).filter(i => primes.takeWhile(p => p * p <= i).forall(i % _ > 0))
}

object run_the_sieve extends App{
//  val inf_primes = TheSieve.get_primes
//  val n_primes = TheSieve.get_n_primes(50000)
////  println("Infinite prime table works: ")
////  inf_primes.take(3000).toList.grouped(10) foreach(n => println(n.mkString(" ")))
//  println("\nFixed size prime table works: ")
//  n_primes.toList.grouped(10) foreach(n => println(n.mkString(" ")))
  println("Trying the new code:")
  val primes: LazyList[Long] = 2L #:: LazyList.from(3).map(_.toLong).filter(i => primes.takeWhile(p => p * p <= i).forall(i % _ > 0))
  //2147483647
  val pt = primes.take(2147483647).toList.grouped(10).map(x => x.mkString(","))

  val file = new File("Primes.csv")
  val bw = new BufferedWriter(new FileWriter(file))
  for (group <- pt) {
    bw.write(group)
    bw.write("\n")
  }
  println(file.getPath)
  bw.close()

}
