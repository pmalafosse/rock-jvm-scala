package playground

import scala.annotation.tailrec

object ScalaPlayground extends App {
  println("Hello scala")


  abstract class MyList[+A] {
    def head: A
    def tail: MyList[A]
    def isEmpty: Boolean
    def add[B >: A](elem: B) : MyList[B]

    def map[B](myTransformer: MyTransformer[A, B]): MyList[B]
    def flatMap[B](myTransformer: MyTransformer[A, MyList[B]]): MyList[B]
    def filter(myFilter: MyPredicate[A]): MyList[A]

    def ++[B >: A](list: MyList[B]): MyList[B]

  }

  object Empty extends MyList[Nothing] {
    def head = throw new NoSuchElementException()
    def tail = throw new NoSuchElementException()
    def isEmpty = true
    def add[A >: Nothing](element: A) = new Cons(element, Empty)

    def ++[B >: Nothing](list: MyList[B]) = list

    def map[B](myTransformer: MyTransformer[Nothing, B]): MyList[B] = this
    def flatMap[B](myTransformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = this
    def filter(myFilter: MyPredicate[Nothing]): MyList[Nothing] = this
  }

  class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
    def head: A = h
    def tail: MyList[A] = t
    def isEmpty = false
    def add[B >: A](element: B) = new Cons(element, this)

    def ++[B >: A](list: MyList[B]): MyList[B] = {
      new Cons(h, t ++ list)
    }

    def map[B](myTransformer: MyTransformer[A, B]): MyList[B] = {
      new Cons(myTransformer.transform(h), t.map(myTransformer))
    }
    def flatMap[B](myTransformer: MyTransformer[A, MyList[B]]): MyList[B] = {
      myTransformer.transform(h) ++ t.flatMap(myTransformer)
    }
    def filter(myFilter: MyPredicate[A]): MyList[A] = {
        if(myFilter.test(h)) new Cons(h, t.filter(myFilter))
        else t.filter(myFilter)
    }
  }

  println(Empty.add(2).add(3).add(4))

  private trait MyPredicate[-T] {
    def test(element: T): Boolean
  }

  private trait MyTransformer[-A, B]{
    def transform(element: A): B
  }





}
