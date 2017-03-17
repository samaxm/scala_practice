/**
  * Created by Sammax on 2017/3/13.
  */
sealed trait Option[+A]{
  def map[B](f:A=>B): Option[B] ={
    this match {
      case Some(v)=>Some(f(v))
      case None=>None
    }
  }

  def flatMap[B](f:A=>Option[B]):Option[B]={
    this match {
      case None =>None
      case Some(v)=>f(v)
    }
  }

  def filter(f: A => Boolean): Option[A]={
    this match{
      case None => None
      case Some(v)=>if(f(v)) Some(v) else None
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B]={
      this match {
        case None=>ob
        case Some(v)=>Some(v)
      }
  }

  def getOrElse[B>:A](default: =>B):B={
    this match {
      case None=>default
      case Some(v)=>v
    }
  }
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]={
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]]={
    a.foldRight(Some(Nil:List[A]):Option[List[A]])((a,list)=>a flatMap( aa=>list.map(l=>aa::l)))
  }
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]={
    sequence(a.map(f))
  }
}
case class Some[A](value:A) extends Option[A]
case object None extends Option[Nothing]


object test{

  def variance(xs: Seq[Double]): Option[Double]={
    mean(xs).flatMap(m=>mean(xs.map(x=>math.pow(x-m,2))))
  }


  def mean(seq: Seq[Double]):Option[Double]={
    if(seq.isEmpty) None
    else Some(seq.sum/seq.length)
  }

  def main(args: Array[String]): Unit = {

  }
}
