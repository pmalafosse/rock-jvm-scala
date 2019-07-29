package playground

object Exceptions extends App {
  // val aWeirdValue : String = throw new NullPointerException

  def fact(elem: Int): BigInt = {
    if(elem==1) 1
    else elem * fact(elem - 1)
  }

  val array = Array.ofDim(Int.MaxValue)
  println(array)
}
