/**
  * Created by Sammax on 2017/3/21.
  */
trait RandomGenerator {
  def nextInt: (Int, RandomGenerator)
  def nextNonNegativeInt: (Int, RandomGenerator)
  def nextDouble: (Double, RandomGenerator)
  def intDouble: ((Int,Double), RandomGenerator)
  def doubleInt: ((Double,Int), RandomGenerator)
  def double3: ((Double,Double,Double), RandomGenerator)
  def ints(count: Int): (List[Int], RandomGenerator)
}

case class SimpleRNG(seed: Long) extends RandomGenerator {

  override def nextInt: (Int, RandomGenerator) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nextNonNegativeInt: (Int, RandomGenerator)={
      val (n,s)=nextInt
      (if (n < 0) -(n + 1) else n, s)
  }

  override def nextDouble: (Double, RandomGenerator)={
    val (n,s)=nextNonNegativeInt
    (n/Int.MaxValue.toDouble,s)
  }

  override def intDouble: ((Int, Double), RandomGenerator) = {
    val (n,r)=nextInt
    val (d,r1)=r.nextDouble
    ((n,d),r1)
  }

  override def doubleInt: ((Double, Int), RandomGenerator) = {
    val (d,r1)=nextDouble
    val (n,r)=r1.nextInt
    ((d,n),r)
  }

  override def double3: ((Double, Double, Double), RandomGenerator) = {
    val (d,r)=nextDouble
    val (d1,r1)=r.nextDouble
    val (d2,r2)=r1.nextDouble
    ((d,d1,d2),r2)
  }

  override def ints(count: Int): (List[Int], RandomGenerator) = {
      if(count==0) (Nil,this)
      else {
        val (n,r)=nextInt
        val (n1,r1)=r.ints(count-1)
        (n::n1,r1)
      }
  }

}

object RandomGenerator{
  type Rand[+A] = RandomGenerator => (A, RandomGenerator)
  def int:Rand[Int]=nextInt
  def double:Rand[Double]=nextDouble
  def intDouble:Rand[(Int,Double)]=both(int,double)
  def doubleInt:Rand[(Double,Int)]=both(double,int)
  def nextInt(r:RandomGenerator): (Int, RandomGenerator) = {r.nextInt}
  def unit[A](a:A):Rand[A]=rng=>(a,rng)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]=rng=>g(f(rng)._1)(rng)
  def map[A,B](r:Rand[A])(f:A=>B):Rand[B]=flatMap(r)(a=>rng=>(f(a),rng))
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]=flatMap[(A,B),C](r=>((ra(r)._1,rb(r)._1),r))(e=>rng=>(f(e._1,e._2),rng))
  def nextNonNegative(r:RandomGenerator):(Int, RandomGenerator)=r.nextNonNegativeInt
  def nextDouble(r:RandomGenerator): (Double, RandomGenerator)=map(nextNonNegative)(v=>(v/Int.MaxValue).toDouble)(r)
  def nextNonNegativeEven(r:RandomGenerator):Rand[Int] = map(nextNonNegative)(v=>v-v%2)
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)]=map2(ra,rb)((_,_))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]= fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  def main(args: Array[String]): Unit = {println(SimpleRNG(123).ints(10)._1)}
}

