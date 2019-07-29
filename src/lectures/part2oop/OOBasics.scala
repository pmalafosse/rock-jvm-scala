package lectures.part2oop

object OOBasics extends App {
  val person = new Person("Pierre", 28)

  println(person.age)
}


class Person(name: String, val age: Int)

