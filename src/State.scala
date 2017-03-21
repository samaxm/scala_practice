/**
  * Created by Sammax on 2017/3/21.
  */
case class State[S,+A](run: S => (A,S)){
  def flatMap[B](f:A=>State[S,B]):State[S,B]= State[S,B](s=>(f(this.run(s)._1).run(s)._1,s))
  def map[B](f:A=>B):State[S,B]= flatMap(a=>State.unit(f(a)))
  def map2[B,C](f:(A,B)=>C)(b:State[S,B]):State[S,C]=  flatMap(a => b.map(b => f(a, b)))
}
object State{
  def unit[S,A](a:A):State[S,A]=new State(s=>(a,s))

  def apply[A,S](run: S => (A, S)): State[S,A] = new State(run)

  def sequence[S,A](as:List[State[S,A]]):State[S,List[A]]= ???
}