package u03

import java.awt.font.TextAttribute
import scala.annotation.tailrec

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] =
      /*case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()*/
      flatMap(l1)(v => if (pred(v)) Cons(v, Nil()) else Nil())


    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h,t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) => Cons(h, t)
      case Nil() => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    import u02.Optionals.*
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= orElse(max(t), h) => Some(h)
      case Cons(h, t) => max(t)
      case Nil() => None()

    import u02.AlgebraicDataTypes.Person.*
    import u02.AlgebraicDataTypes.*
    def getCourseOfPersons(persons: List[Person]): List[String] =
      flatMap(persons)({
        case Teacher(name, course) => Cons(course, Nil())
        case _ => Nil()
      })




    //import List.*
    //val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))

    //println(List.sum(l)) // 60

    //println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

    //getCourseOfPersons(Cons(Teacher("diego", "italiano"), Nil())))), getCourseOfPersons(Cons(Teacher("giacomo", "math"), Cons(Teacher("fabri", "storia"), Cons(Student("mauro", 2021), Cons(Student("fabrizio", 2022) ,Cons(Teacher("diego", "italiano") ,Nil()))))))

