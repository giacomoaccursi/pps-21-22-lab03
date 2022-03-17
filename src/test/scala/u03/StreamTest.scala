package u03

import org.junit.*
import org.junit.Assert.*
import u02.AlgebraicDataTypes.Person.{Student, Teacher}
import u03.Lists.*
import u03.Lists.List.{Cons, Nil, filter}
import u03.Streams.Stream

class StreamTest:
  import Streams.*
  import Streams.Stream.*


  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def tetDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Nil(), Stream.toList(Stream.drop(empty())(2)))

  @Test def tetConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(constant("x"))(5)))

  @Test def testFibonacci() =
    val fibs : Stream [Int ] = fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))


