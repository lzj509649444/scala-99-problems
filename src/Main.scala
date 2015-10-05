/**
 * Created by lzj on 15-10-4.
 */

class A(bb: Int)

object Scala99{

  // P01: Find the last element of a list.

  def mlast[A](xs: List[A]) = xs.last

  def mlast2[A](xs: List[A]): Option[A] = {
    xs match {
      case head::Nil => Some(head)
      case head::tail => mlast2(tail)
      case Nil => None
    }
  }

  def mlast3[A](l:List[A]):A = l match {
    case h :: Nil => h
    case _ :: tail => mlast3(tail)
    case _ => throw new NoSuchElementException
  }

  // P02: Find the last but one element of a list.

  def lastbutone(xs: List[Int]): Int = xs match{
    case x :: y :: Nil => x
    case x :: y :: tail => lastbutone(y::tail)
    case _ => throw new Exception
  }

  //P03: Find the Kth element of a list.
  def kth(k: Int,xs: List[Int]): Int ={
    var l =  xs.zipWithIndex
    var ll = for((a,i) <- l if i < k) yield a
    mlast3(ll)
  }

  def kth2(k: Int,xs: List[Int]): Int = k match {
    case 0 => xs.head
    case x if x < 0 || x > xs.length - 1 => throw new Exception
    case _ => kth2(k-1,xs.tail)
  }


  // P04: Find the number of elements of a list.
  def findn(n: Int,xs: List[Int]): Boolean = {
    for(a <- xs if a == n) return true
    false
  }

  def len(xs: List[Int]): Int = xs match {
    case Nil => 0
    case h :: tail => 1 + len(tail)
  }

  def len2(xs: List[Int]) = {
    def lenn(n: Int,xs: List[Int]) : Int = xs match {
      case Nil => n
      case h :: tail => lenn(n+1,tail)
    }
    lenn(0,xs)
  }

  // P05: Reverse a list.
  def reverse(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: tail => reverse(tail) :+ h
  }

  def reverse2(xs: List[Int]): List[Int] = xs match{
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: tail => reverse2(tail) ::: List(h)
  }

  // P06: Find out whether a list is a palindrome.
  def isPalindrome(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case h :: Nil => true
    case _ => xs.head == xs.last && isPalindrome(xs.tail.reverse.tail.reverse)
  }

  def isPalindrome2(xs: List[Int]): Boolean = {
    def loop(b:Int,e:Int,xs:List[Int]): Boolean = {
      if(b >= e)return true
      xs(b) == xs(e) && loop(b+1,e-1,xs)
    }

    loop(0,xs.length -1,xs)
  }

  // P07: Flatten a nested list structure.
  def flatten(xs: Any): List[Any] = xs match {
    case Nil => Nil
    case q => List(q)
    case h :: tail => flatten(h) ::: flatten(tail)
  }

}



object Main {
  def main(args: Array[String]) {
    println(Scala99.kth(1,List(1,2,3)))
    println(Scala99.kth(2,List(1,2,3)))
    println(Scala99.kth(-1,List(1,2,3)))
    println(Scala99.kth(5,List(1,2,3)))
  }
}
