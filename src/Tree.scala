/**
  * Created by Sammax on 2017/3/10.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l, r) => size(l) + size(r))
  }

  def searchMax(t: Tree[Int]): Int = {
    fold[Int, Int](t)(a => a)((l, r) => searchMax(l) max searchMax(r))
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l, _) => depth(l) + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A,Tree[B]](t)(a=>Leaf(f(a)))((l,r)=>Branch(map(l)(f),map(r)(f)))
  }

  def fold[A, B](t: Tree[A])(g: A => B)(f: ((Tree[A], Tree[A]) => B)): B = {
    t match {
      case Leaf(a) => g(a)
      case Branch(l, r) => f(l, r)
    }
  }

  def main(args: Array[String]): Unit = {
    val data = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(2)))
    println(map(data)(_*3))
  }

}