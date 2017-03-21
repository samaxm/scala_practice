/**
  * Created by Sammax on 2017/3/17.
  */
sealed trait Stream[+A] {

  def toList:List[A]= {
    println("print")
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }
  def take(n:Int):Stream[A]=this match {
      case Cons(h, t) if n>0 => Stream.cons(h(), t().take(n - 1))
      case _ => Empty
  }

  def drop(n:Int):Stream[A] =this match {
    case Cons(_,t) if n>0=>t().drop(n-1)
    case _=>this
  }

  def foldRight[B](z: =>B)(f: ( A, =>B)=>B):B= {
   println("call fold")
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def foldRight2[B](z: =>B)(f: ( A, =>B)=>B):B= {
    println("call fold2")
    this match {
      case Cons(h, t) => f(h(), t().foldRight2(z)(f))
      case _ => z
    }
  }

  def exist(p:(A=>Boolean)):Boolean={
      foldRight(false)((a,b)=>p(a)||b)
  }

  def forAll(p:(A=>Boolean)):Boolean={
    foldRight(false)((a,b)=>p(a)&&b)
  }

  def takeWhile(p: A => Boolean): Stream[A]={
    foldRight(Empty:Stream[A])((a,b)=> if(p(a)) Stream.cons(a,b) else Empty:Stream[A])
  }

  def headOption():Option[A]={
    foldRight(None:Option[A])((a,_)=>Some(a))
  }

  def map[B](m:A=>B):Stream[B]={
    foldRight(Empty:Stream[B])((a,b)=>Stream.cons(m(a),b))
  }

  def filter(p:A=>Boolean):Stream[A]={
    foldRight2(Empty:Stream[A])((a,b)=> if(p(a)) Stream.cons(a,b) else b)
  }
  def append[B>:A](a: =>Stream[B]):Stream[B]={
    foldRight(a)((h,t) =>Stream.cons(h,t))
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] ={
    foldRight(Empty:Stream[B])((a,b)=>b.append(f(a)))
  }


}
case object Empty extends Stream[Nothing]
case class Cons[A](head:()=>A,tail:()=>Stream[A]) extends Stream[A]
object Stream{
  def cons[A](a: =>A,b: =>Stream[A]):Stream[A]={
    lazy val aa=a
    lazy val bb=b
    Cons(()=>aa,()=>bb)
  }
  def empty[A]():Stream[A]=Empty

  def apply[A](a:A*): Stream[A] ={
    if(a.isEmpty) Empty
    else {
      cons(a.head,apply(a.tail:_*))
    }
  }

  def headOption[A](s:Stream[A]):Option[A]={
    s match {
      case Empty=>None
      case Cons(h,t)=>Some(h())
    }
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]={
    f(z) match {
      case None=>Empty
      case Some(v)=>Stream.cons(v._1,unfold(v._2)(f))
    }
  }
  def constant[A](a: A): Stream[A]={
    unfold(a)(a=>Some((a,a)))
  }
  def from(n: Int): Stream[Int]={
    unfold(n)(n=>Some((n,n+1)))
  }

  def fibs():Stream[Int]={
    unfold((0,1))(n=>Some((n._1,(n._2,n._1+n._2))))
  }



  def main(args: Array[String]): Unit = {
    println(Stream(1,5).map(_+4).filter(_<7).toList)
  }
}