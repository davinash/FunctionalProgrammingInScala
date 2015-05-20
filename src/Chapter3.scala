import scala.annotation.tailrec

/**
 * Created by adongre on 19/5/15.
 *
 */


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    /*
     * EXERCISE 2: Implement the function tail for "removing" the first element
     * of a List. Notice the function takes constant time. What are different choices you
     * could make in your implementation if the List is Nil? We will return to this
     * question in the next chapter.
     */
    def tail[A](list: List[A]): List[A] = list match {
        case Nil => Nil // This can be changed to throw Exception.
        case Cons(x, xs) => xs
    }

    /*
     * EXERCISE 3: Generalize tail to the function drop, which removes the first
     * n elements from a list.
     */
    def dropWithPatternMatching[A](l: List[A], n: Int): List[A] = l match {
        case Nil => throw new RuntimeException("Invalid Input")
        case Cons(x, xs) => {
            if (n - 1 == 0) xs
            else
                dropWithPatternMatching(xs, n - 1)
        }
    }

    def drop[A](l: List[A], n: Int): List[A] = {
        if (n <= 0) l
        else drop(tail(l), n - 1)

    }

    /*
     * EXERCISE 4: Implement dropWhile, which removes elements from the
     * List prefix as long as they match a predicate. Again, notice these functions take
     * time proportional only to the number of elements being dropped—we do not need
     * to make a copy of the entire List.
     *
     */

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Nil => throw new RuntimeException("Invalid Input")
        case Cons(x, xs) => if (f(x)) dropWhile(xs)(f)
        else l
    }

    /*
     * EXERCISE 5: Using the same idea, implement the function setHead for
     * replacing the first element of a List with a different value.
     */

    def setHead[A](l:List[A], newValue: A) : List[A] = l match {
        case Nil => Cons(newValue, l)
        case Cons(x,xs) => Cons(newValue, xs)
    }

    /*
     * EXERCISE 6: Not everything works out so nicely. Implement a function,
     * init, which returns a List consisting of all but the last element of a List. So,
     * given List(1,2,3,4), init will return List(1,2,3). Why can't this
     * function be implemented in constant time like tail?
     */

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_,Nil) => Nil
        case Cons(x,xs)  => Cons(x, init(xs))

    }
}

object Chapter3 {

    def main(args: Array[String]): Unit = {
        val example = Cons(1, Cons(2, Cons(3, Nil)))
        val example2 = List(1, 2, 3)
        val total = List.sum(example)

        val x = List(1, 2, 3, 4, 5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + List.sum(t)
            case _ => 101
        }
        println(x)

        println("tail                      = " + List.tail(List(1, 2, 3, 4, 5)))
        println("dropWithPatternMatching   = " + List.dropWithPatternMatching(List(1, 2, 3, 4, 5), 2))
        println("drop                      = " + List.drop(List(1, 2, 3, 4, 5), 2))
        println("DropWhile                 = " + List.dropWhile(List(1, 2, 3, 4, 5))((x) => x == 4))
        println("DropWhile                 = " + List.dropWhile(List(1, 3, 4, 0, -1, 6))(_ > 0))
        println("setHead                   = " + List.setHead(List(1, 3, 4, 0, -1, 6), 10))
        println("init                      = " + List.init(List(1,2,3,4,5)))

    }
}
