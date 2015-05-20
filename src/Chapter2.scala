import scala.annotation.tailrec

/**
 * Created by adongre on 19/5/15.
 *
 * EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. The
 * first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
 * the previous two. Your definition should use a local tail-recursive function.
 */
object Chapter2 {
    def factorial(n: Int): Int = n match {
        case 0 => 1
        case 1 => 1
        case _ => n * factorial(n - 1)
    }

    def factorial2(n: Int): Int = {

        @tailrec
        def loop(n: Int, accumulator: Int): Int = {
            if (n <= 0) accumulator
            else loop(n - 1, accumulator * n)
        }
        loop(n, 1)
    }

    /*
     * EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
     * sorted according to a given comparison function.
     */

    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

        @tailrec
        def loop(index: Int): Boolean = {
            if (index >= as.length - 1) true
            else if (!gt(as(index), as(index + 1))) false
            else loop(index + 1)

        }
        loop(0)
    }


}
