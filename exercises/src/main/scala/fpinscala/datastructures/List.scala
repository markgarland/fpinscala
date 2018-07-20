package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception ("Tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Exception ("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case n if n < 0 => throw new Exception ("negative n")
    case n => drop (tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhileInferred[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhileInferred(t)(f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("init on Nil")
    case Cons(_,Nil) => Nil
    case Cons(h, t) => Cons(h, init (t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftSum(ns: List[Int]) =
    foldLeft(ns, 0)(_+_)

  def foldLeftProduct(ns: List[Int]) =
    foldLeft(ns, 1)(_*_)

  def foldLeftLength(ns: List[Int]) =
    foldLeft(ns, 0)((_, y) => 1 + y)

  def reverse(ns: List[Int]) = foldLeft(ns, Nil: List[Int])((x, y) => Cons(y,x))

  def appendFolded[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((x, y) => Cons(y, x))

  def concat[A](a1: List[List[A]]): List[A] = foldRight(a1,  Nil: List[A])((x,y)=>appendFolded(x,y))

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, y) => Cons(x.toString, y))

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterUsingFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((x) => if (f(x)) Cons(x, Nil) else Nil)

  def addFunction(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(l1x,l1y), Cons(l2x, l2y)) => Cons(l1x+l2x, addFunction(l1y, l2y))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(l1x,l1y), Cons(l2x, l2y)) => Cons(f(l1x,l2x), zipWith(l1y, l2y)(f))
  }

  def main(args: Array[String]): Unit = {
    println (x)

    val list: List[Int] = List(1,2,3,4)
    val list2: List[Int] = List(6,7,8,9)
    val listDouble: List[Double] = List(1.0, 2.0, 3.0, 4.0)

    println (list)
    println ("tail: " + tail(list))
    println ("setHead: " + setHead(list, 4))
    println ("drop: " + drop(list, 2))
    println ("dropWhile: " + dropWhile(list, (x: Int) => x <= 2))
    println ("dropWhileInferred: " + dropWhileInferred(list)(x => x <= 2))
    println ("init: " + init(list))

    println ("foldRight1: " + foldRight(list, 0)((x, y) => x + y))
    println ("foldRight2: " + foldRight(list, Nil:List[Int])(Cons(_,_)))
    println ("length: " + length(list))
    println ("foldLeft: " + foldLeft(list, 0)((x, y) => x + y))
    println ("foldLeftSum: " + foldLeftSum(list))
    println ("foldLeftProduct: " + foldLeftProduct(list))
    println ("foldLeftLength: " + foldLeftLength(list))
    println ("reverse: " + reverse(list))

    println ("appendFolded: " + appendFolded(list, list2))
    println ("concat: " + concat(List(list, list2, list, list2)))

    println ("addOne: " + addOne(list))
    println ("doubleToString: " + doubleToString(listDouble))
    println ("map: " + map(listDouble)(x => x.toString + "mapped"))
    println ("filter: " + filter(list)(x => x % 2 == 0))
    println ("flatMap: " + flatMap(list)(i => List(i,i)))
    println ("filterUsingFlatmap: " + filterUsingFlatmap(list)(x => x % 2 == 0))

    println ("addFunction: " + addFunction(list,list2))
    println ("zipWith: " + zipWith(list,list2)(_+_))


  }

}
