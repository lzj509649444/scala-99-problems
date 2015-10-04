/**
 * Created by lzj on 15-10-4.
 */
// P01: Find the last element of a list.
object Scala99{
  def mlast[A](xs: List[A]) = xs.last
}



object Main {
  def main(args: Array[String]) {
    println(Scala99.mlast(List(1,2,3)))
  }
}
