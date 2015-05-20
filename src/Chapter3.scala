import scala.annotation.tailrec

/**
 * Created by adongre on 19/5/15.
 *
 * EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
 * sorted according to a given comparison function.
 *
 */
object Chapter3 {
    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

        @tailrec
        def loop(index: Int): Boolean = {
            if (index >= as.length - 1) true
            else if (!gt(as(index), as(index + 1))) false
            else loop(index + 1)

        }
        loop(0)
    }


    def main(args: Array[String]): Unit = {
        println(isSorted[Int](Array[Int](1, 2, 3, 4, 5, 6, 7), (x: Int, y: Int) => x <= y))
        println(isSorted[Int](Array[Int](10, 2, 3, 4, 5, 6, 7), (x: Int, y: Int) => x <= y))
    }
}
