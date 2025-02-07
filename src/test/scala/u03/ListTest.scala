package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.AlgebraicDataTypes.Person.{Student, Teacher}
import u02.Optionals.Option.Some
import u03.Lists.List.{Cons, Nil, filter}

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))

  @Test def testAppend() =
    val tail = Cons(40,Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31 ,Cons(32 ,Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax() =
    import u02.Optionals.*
    import u02.Optionals.Option.*
    assertEquals(Some(25), max(Cons(10 ,Cons(25 ,Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  @Test def testGetCourseOfPersons() =
    import u02.AlgebraicDataTypes.Person.*
    assertEquals(Cons("math",Cons("storia", Cons("italiano", Nil()))), getCourseOfPersons(Cons(Teacher("giacomo", "math"), Cons(Teacher("fabri", "storia"), Cons(Student("mauro", 2021), Cons(Student("fabrizio", 2022) ,Cons(Teacher("diego", "italiano") ,Nil())))))))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testFoldRight() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
