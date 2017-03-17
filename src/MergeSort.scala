/**
  * Created by Sammax on 2017/3/1.
  */
object MergeSort {


  def merge[T](list:List[T])(compare:(T,T)=>Boolean):List[T]={
    val n=list.length/2
    if(n==0) list
    else{
      def merge2(left:List[T],right:List[T]):List[T]={
        (left,right) match {
          case (Nil,right)=> right
          case (left,Nil)=> left
          case (lh::lr,rh::rr)=>{
            if(compare(lh,rh)){
              rh::merge2(left,rr)
            }else{
              lh::merge2(lr,right)
            }
          }
        }
      }
      val (left,right)=list splitAt n
      merge2(merge(left)(compare),merge(right)(compare))
    }
  }


  def main(args: Array[String]): Unit = {
    val list=List(4,6,1,3,2)
    val list2=merge(list)((a:Int,b:Int)=> a>b)
    print( list2)
  }
}
