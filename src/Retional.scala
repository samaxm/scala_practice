/**
  * Created by Sammax on 2017/2/23.
  */
class Retional(val a:Int,val b:Int) {

  val _a=a/gcd(a,b)
  val _b=b/gcd(a,b)

  def gcd(a:Int,b:Int):Int={
    if(a<b) gcd(b,a)
    else if (a%b==0) b
    else gcd(b,a%b)
  }

  override def toString():String={
      _a+"/"+_b
  }
  def + (retional: Retional)={
    new Retional(this._a*retional._b+retional._a*this._b,this._b*retional._b)
  }

  def - (retional: Retional)={
    new Retional(this._a*retional._b-retional._a*this._b,this._b*retional._b)
  }

  def * (retional: Retional)={
    new Retional(this._a*retional._a,this._b*retional._b)
  }

  def / (retional: Retional)={
    *(new Retional(retional.b,retional.a))
  }
}

object Retional{
  def apply(a:Int,b:Int):Retional=new Retional(a,b)
  def gcd(a:Int,b:Int):Int={
    if(a<b) gcd(b,a)
    else if (a%b==0) b
    else gcd(b,a%b)
  }
  def main(args: Array[String]): Unit = {
    println(Retional(2,3))
    println(Retional(2,3) + Retional(2,8))
    println(Retional(2,3) - Retional(1,5))
    println(Retional(2,3) * Retional(1,4))
    println(Retional(2,3) / Retional(7,1))

  }

}
