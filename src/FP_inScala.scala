/**
  * Created by Sammax on 2017/3/8.
  */
object FP_inScala {

  def fib(n:Int):Int={
    if (n==0) throw new IllegalArgumentException
    def accFib(m:Int):Int={
      if(m==1) 0
      else if (m==2) 1
      else accFib(m-1)+accFib(m-2)
    }
    accFib(n)
  }

  def foldRight[A,B](list: List[A],zero:B)(f:(A,B)=>B):B={
    foldLeft(reverse(list),zero)((a,b)=>f(b,a))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B={
    as match {
      case Nil=>z
      case h::t=>foldLeft(t,f(z,h))(f)
    }
  }

  def append[A](a: List[A],b: List[A]):List[A]={
      foldRight(a,b)((ele,b)=>ele::b)
  }

  def concatenates[A](list:List[List[A]]):List[A]={
    foldLeft(list,Nil:List[A])((container,as)=>append(container,as))
  }

  def length[A](as: List[A]): Int={
      foldLeft[A,Int](as,0)((a,b)=>a+1)
  }

  def reverse[A](as:List[A]):List[A]={
    foldLeft(as,Nil:List[A])((list,ele)=>ele::list)
  }

  def addOne(list:List[Int]):List[Int]={
    foldLeft(list,Nil:List[Int])((result,ele)=>ele+1::result)
  }

  def double2String(list: List[Double]):List[String]={
    foldLeft(list,Nil:List[String])((list,ele)=>ele.toString::list)
  }


  def map[A,B](as: List[A])(f: A => B): List[B]={
    foldLeft(as,Nil:List[B])((transfer,ele)=>f(ele)::transfer)
  }

  def filter[A](as:List[A])(test:A=>Boolean):List[A]={
//    foldLeft(as,Nil:List[A])((transfer,ele)=>if(test(ele)) ele::transfer else transfer)
    flatMap(as)(a=>if(test(a)) List(a) else Nil)
  }


  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]={
    foldLeft(as,Nil:List[B])((resp,ele)=>append(resp,f(ele)))
  }

  def zipWith[A](a:List[A],b:List[A])(f:(A,A)=>A):List[A]= {
    def acc(a: List[A], b: List[A], c: List[A]): List[A] = {
      (a, b) match {
        case (ah :: at, bh :: bt) => acc(at, bt, f(ah, bh) :: c)
        case (ah :: at, Nil) => acc(at, Nil, ah :: c)
        case (Nil, bh :: bt) => acc(Nil, bt, bh :: c)
        case (Nil, Nil) => c
      }
    }
    acc(a,b,Nil)
  }
  def addPair(a:List[Int],b:List[Int]):List[Int]={
    def acc(a:List[Int],b:List[Int],c:List[Int]):List[Int]={
      (a,b) match {
        case (ah::at,bh::bt)=> acc(at,bt,ah+bh::c)
        case (ah::at,Nil)=>acc(at,Nil,ah::c)
        case (Nil,bh::bt)=>acc(Nil,bt,bh::c)
        case (Nil,Nil)=>c
      }
    }
      acc(a,b,Nil)
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean={
    def check(a:List[A],b:List[A]):Boolean={
      (a,b) match {
        case (ah::at,bh::bt)=> if(ah==bh) check(at,bt) else false
        case (_,Nil) =>true
        case (Nil,_)=>false
      }
    }
    def loop(a:List[A]):Boolean={
      a match {
        case Nil=>false
        case h::t=> if(check(h::t,sub)) true else loop(t)
      }
    }
    loop(sup)
  }


  def main(args: Array[String]): Unit = {
    val data=List(1,2,3,4,5)
    val data2=List(6,7,8,3)
    val d=List(1)
    println(hasSubsequence(data,d))
//    val data4=List(1.0,2.0,3.3)
//    val data3=List(data,data2)
//    println(flatMap(List(1,2,3))(i => List(i,i)))
//    val result=FP_inScala.foldRight(data,0)(_+_)
//     println(addPair(data,data2))
//    println(concatenates(data3))
//    println(addOne(data))
//    println(double2String(data4))
//    println(map(data)(_*2))
//    val result2=FP_inScala.foldLeft(data,1)(_*_)
//    println(FP_inScala.length(data))
//    println(FP_inScala.reverse(data))


  }
}
