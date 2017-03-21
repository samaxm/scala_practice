import java.util.Scanner

import scala.collection.immutable.{HashMap, HashSet}

/**
  * Created by admin on 2017/2/16.
  */
class School {


  def addStudent(name:String,grade:Int,students:HashMap[String,Int],gradeIndex:HashMap[Int,HashSet[String]]):(HashMap[String,Int],HashMap[Int,HashSet[String]])={
    val as=students + (name->grade)
    if (gradeIndex.get(grade).isEmpty)
      (as,gradeIndex + (grade->HashSet(name)))
    else
      (as,gradeIndex updated (grade,gradeIndex.get(grade).get + name))
  }

  def removeStudent(name:String,students:HashMap[String,Int],gradeIndex:HashMap[Int,HashSet[String]]):(HashMap[String,Int],HashMap[Int,HashSet[String]])={
    val record=students.get(name)
    if(record.isEmpty){
        (students,gradeIndex)
      }else{
        (students - name,gradeIndex updated (record.get,gradeIndex.get(record.get).get - name))
      }
  }

  def updateStudent(name:String,grade:Int,students:HashMap[String,Int],gradeIndex:HashMap[Int,HashSet[String]]):(HashMap[String,Int],HashMap[Int,HashSet[String]])={
    if(!students.contains(name)){
      addStudent(name,grade,students,gradeIndex)
    }else{
      val dele=removeStudent(name,students,gradeIndex)
      addStudent(name,grade,dele._1,dele._2)
    }
  }


  def queryStudent(name: String,students:HashMap[String,Int]):(String,Int)={
    if(students.contains(name)){
      (name,students.get(name).get)
    }else{
      ("",-1)
    }
  }

  def queryGrade(grade:Int,gradeIndex:HashMap[Int,HashSet[String]]):HashSet[String]={
    if(gradeIndex.contains(grade)){
      gradeIndex.get(grade).get
    }else{
      new HashSet[String]
    }
  }
}

object Command extends Enumeration{
  type Command=Value
  val ADD,DELETE,UPDATE,QUERY,LIST,EXIT=Value

}

object Week5{

  def apply: School = new School()
  val scanner=new java.util.Scanner(System.in)
  val school=apply
  import Command._

  def bootup(students:HashMap[String,Int],gradeIndex:HashMap[Int,HashSet[String]]):Unit={
    def process(students:HashMap[String,Int],gradeIndex:HashMap[Int,HashSet[String]]):(HashMap[String,Int],HashMap[Int,HashSet[String]])= {
      if (scanner.hasNextLine) {
        val args = scanner.nextLine().split(" ")
        if (args.length < 1 || args.length > 3) {
          println("illegal argument, format:<command arg1 arg2>")
          (students, gradeIndex)
        }
        else {

          try {

            Command.withName(args(0).toUpperCase) match {
              case ADD =>
                println("add student:" + args(1) + " grade:" + args(2))
                school.addStudent(args(1), args(2).toInt, students, gradeIndex)
              case DELETE =>
                println("delete student:" + args(1))
                school.removeStudent(args(1), students, gradeIndex)
              case UPDATE =>
                println("update student:" + args(1) + " grade:" + args(2))
                school.updateStudent(args(1), args(2).toInt, students, gradeIndex)
              case QUERY =>
                println("query student:" + args(1) + " result:" + school.queryStudent(args(1), students))
                (students, gradeIndex)
              case LIST =>
                println("list grade " + args(1) + " students" + school.queryGrade(args(1).toInt, gradeIndex))
                (students, gradeIndex)
              case EXIT =>
                sys.exit()
              case _ =>
                println("unsupport command@" + args(0))
                (students, gradeIndex)
            }
          }catch{
            case e:Exception=>
              println("usage : \n add name grade \n delete name \n update name grade \n query name \n list grade \n exit ")
              (students, gradeIndex)
          }
        }
      }else{
        (students, gradeIndex)
      }
    }
    val result=process(students,gradeIndex)
    bootup(result._1,result._2)
  }


  def main(args: Array[String]): Unit = {
    bootup(new HashMap[String,Int],new HashMap[Int,HashSet[String]])
//    print(ADD.toString)
    val list=List()
    list.flatten
  }
  
  
}
