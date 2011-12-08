package me.rémi.s99

import java.util.NoSuchElementException

class S199List(n: Int, a: Any) {
  var element1: Int = n
  var element2: Any = a
  def this(a: Any) = this(1,Nil)

  override def toString = element1+";"+element2
}

object S199List {

  def main(args: Array[String]) {
    println(sortFreq(List(List(3,4,5),List(2,9), List(5,6,4,23,8),List(3,4),List(4,5,6),List(5,6,7))))
  }

  def lastList[A](l: List[A]): A = {               //nice type thing, not the same as Any
    if (l==List()) throw new IllegalArgumentException("empty list")
    else if (l.length==1) l.head
    else lastList(l.tail)
  }

  def lastListMatch[A](l: List[A]): A = l match{
    case h::Nil => h
    case _::tail => lastListMatch(tail)
    case _ => throw new NoSuchElementException
  }

  def lastButOneList[A](l: List[A]): A = l match {
    case h::k::Nil => h
    case h::tail => lastButOneList(tail)
    case _ => throw new NoSuchElementException
  }

  def kThList[A](l: List[A], k: Int): A = (l, k) match {
    case (Nil,_) => throw new NoSuchElementException
    case (_,1)=>l.head
    case (_,_)=> kThList(l.tail, k-1)
  }

  def listLength[A](l: List[A]): Int = l match{
    case Nil => 0
    case _ => 1+listLength(l.tail)
  }

  def reverseList[A](l: List[A]): List[A] = l match{
    case Nil => Nil
    case _ => (reverseList(l.tail):::(List(l.head)))
  }

  def reverseTailList[A](l: List[A]): List[A] = {
    def reverseR(result: List[A], currList: List[A]): List[A] = currList match {
      case Nil => result
      case h::tail => reverseR(h::result, tail)
    }
    reverseR(List(),l)
  }

  def palindrome[A](l: List[A]): Boolean = l == reverseTailList(l)

  def flatten(l: List[Any]): List[Int] = {
    val str = l.toString()
    var flat = List[Int]()
    for (ch <- str){
      if (ch=='1'||ch=='2'||ch=='3'||ch=='4'||ch=='5'||ch=='6'||ch=='7'||ch=='8'||ch=='9'||ch=='0')
        flat = (ch.toString.toInt)::flat
    }
    reverseTailList(flat)
  }

  def flattenBis(l: List[Any]): List[Any] = {
    if (l==Nil) Nil
    else{
      if ((l.head).isInstanceOf[List[Any]]){
      l //flattenBis(l.head):::flattenBis(l.tail)
      }
      else l.head::flattenBis(l.tail)
    }
  }     //type inference and ifs don't work out so well, use case

  def flattenAgain(l: List[Any]): List[Any] = l match{
    case Nil => Nil
    case (h: List[Any])::tail => flattenAgain(h):::flattenAgain(tail)
    case h::tail => h::flattenAgain(tail)
  }

  def eliminate(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case h::k::tail if h==k=> eliminate(h::tail)
    case h::tail => h::eliminate(tail)
  }

  def subList(l: List[Any]): List[List[Any]] = {
    def subListR(result: List[List[Any]], currList: List[Any], currBuff:List[Any]): List[List[Any]] = (currList,currBuff) match {
      case (Nil,_) => result:::List(currBuff)
      case (h::tail, List()) => subListR(result,tail,List(h))
      case (h::tail, cb) if cb.contains(h) => subListR(result,tail,h::cb)
      case (h::tail, cb) => subListR(result:::List(currBuff), tail, List(h))
    }
    subListR(List(),l,List())
  }

  def runLength(l: List[Any]): List[(Int, Any)] = {
    val newL = subList(l)
    var derL = List[(Int, Any)]()
    for (li <- newL)
      derL = (li.length, li.head)::derL
    derL.reverse
  }

  def runLengthBis(l: List[Any]): List[(Int, Any)] = {
    def runLengthBisR(newL: List[List[Any]]): List[(Int,Any)] = {
      if (newL==Nil) Nil
      else (newL.head.length, newL.head.head)::runLengthBisR(newL.tail)
    }
    runLengthBisR(subList(l))
  }

  def modifiedRunLength[A](l: List[A]): List[Any] = {
    val newL = subList(l)
    var derL = List[Any]()
    for (li <- newL){
      if (li.length!=1) derL = (li.length, li.head)::derL
      else derL = (li.head)::derL
    }
    derL.reverse
  }

  def rLEL(l: List[(Int, Any)]): List[Any] = l match{
    case Nil => Nil
    case h::tail if h._1>0 => h._2::rLEL((h._1-1,h._2)::tail)
    case h::tail => rLEL(tail)
  }

  def runLengthDirect(l:List[Any]): List[(Int, Any)] = {
    def runLengthDirectR(result: List[(Int,  Any)], curList: List[Any], curr: Any,  compt: Int): List[(Int,  Any)] = (curList, curr,  compt) match{
      case (Nil,c,com) => {println(1); result:::List((com,c))}
      case (h::tail,_,com) if com==0 => {println(2); runLengthDirectR(result,tail,h,1)}
      case (h::tail,c,com) if h==c => {println(3); runLengthDirectR(result,tail,c,com+1)}
      case (h::tail,c,com) => {println(4); runLengthDirectR(result:::List((com, c)),tail,h,1)}
    }
    runLengthDirectR(List[(Int, Any)](),l,Nil,0)
  }

  def duplicateList(l: List[Any]): List[Any] = {
    if (l==Nil) Nil
    else l.head::l.head::duplicateList(l.tail)
  }

  def duplicateN(n: Int, l: List[Any]): List[Any] = {
    def duplicateR(result: List[Any], curList: List[Any], num:Int): List[Any] = (curList, num) match{
      case (Nil,_) => {println(1); result}
      case (h::tail,0) => {println(2); duplicateR(result, tail, n)}
      case (h::tail,nume) => {println(3); duplicateR(result:::List(h), h::tail, nume-1)}
    }
    duplicateR(List[Any](),l,n)
  }

  def dropN(l: List[Any], n: Int): List[Any] = {
    def dropR(result: List[Any], curList: List[Any], num: Int): List[Any] = (curList,num) match {
      case (Nil,_) => result
      case (h::tail,1) => dropR(result,tail,n)
      case (h::tail, nume) => dropR(result:::List(h), tail, nume-1)
    }
    dropR(List[Any](),l,n)
  }

  def split(l: List[Any], n: Int): (List[Any],List[Any])= {

    def splitR(result: List[Any], curList: List[Any], num: Int): (List[Any], List[Any]) = (curList,num) match{
      case(lis,0) => (result, lis)
      case(h::tail,nume) => splitR(result:::List(h),tail, nume-1)
    }
    if (l.length<n) throw new Exception("List too short")
    else splitR(List[Any](),l,n)
  }

  def splitRecu(l: List[Any], n:Int): (List[Any], List[Any]) = (l, n) match {
    case (li,0) => (List[Any](),li)
    case (Nil,_) => throw new Exception("List too short")
    case (h::tail,num) => {
      val listNum = splitRecu(tail,num-1)
      (h::listNum._1,listNum._2)
    }
  }

  def sliceRecu(l: List[Any], i: Int,  k: Int): List[Any] = (l, i, k) match {
    case(_,j,m) if (m<j) => throw new Exception("Indexes in the wrong order")
    case(_,_,1) => List[Any]()
    case(Nil,_,_) => throw new Exception("List too short")
    case(h::tail,j,m) if (j>1) => sliceRecu(tail,j-1,m-1)
    case (h::tail,_,m) => h::sliceRecu(tail,0,m-1)
  }

  def rotateRecu(l: List[Any], n:Int): List[Any] = (l, n) match {
    case (li,0) => {println(1); li}
    case (h::tail,num) if (num>0&&num<(h::tail).length) => {println(2); rotateRecu(tail:::List(h),num-1)}
    case (h::tail,num) if (num>0) => {println(3); rotateRecu(h::tail, num%l.length)}
    case (li,num) => {println(4);println(li.length+num%li.length);  rotateRecu(li,li.length+num%li.length)}
  }

  def removeRecu(l: List[Any], k: Int): (List[Any], Any) = (l, k) match {
    case (Nil,_) => throw new Exception("List too short")
    case (h::tail,0) => (tail,h)
    case (h::tail,num) => {
      val remo = removeRecu(tail,num-1)
      (h::remo._1,remo._2)
    }
  }

  def insertRecu(l: List[Any], a: Any, k: Int): List[Any] = (l, k) match {
    case (li,0) => a::li
    case (Nil,_) => throw new Exception("List too short")
    case (h::tail,num) => h::insertRecu(tail,a,num-1)
  }

  def rangeRecu(j: Int,  k: Int): List[Int] = (j, k) match {
    case (i,n) if (n<i) => throw new Exception("indexes in the wrong order")
    case (i,n) if (i==n) => List(i)
    case (i,n) => i::rangeRecu (i+1,n)
  }

  def extract(l:List[Any], n: Int):(List[Any],List[Any]) = {
    var li = l
    var result = List[Any]()
    for (i<-0 to (n-1)){
      val r = (scala.math.random*li.length).toInt
      val lis = removeRecu(li,r)
      li=lis._1
      result = lis._2::result
    }
    (li, result)
  }

  def lotto(n: Int, i: Int): List[Any] = {
    val l = rangeRecu(1,n)
    if (i>n) throw new Exception("too many draws")
    else{
      extract(l,i)._2
    }
  }

  def randomPermutation(l: List[Any]): List[Any] = {
    extract(l,l.length)._2
  }

  def testRandom(n: Int, compteur: Int)={
    val tableau = new Array[Int](n+1)
    for(i<-1 to  compteur){
      val r = (scala.math.random*n).toInt
      tableau(r)+=1
    }
    for (i<-0 to n-1) println(tableau(i))
  }

  //def generateCombinations(l: List[Any], n: Int): List[List[Any]] = {
    //def generateCombinationsR(result: List[Any], curList:List[Any], n: Int,  i: Int): List[Any] = (curList,n,i) match{
      //case (_,0,_) => result
      //case (Nil,_,_) => throw new Exception("List too short")
      //case (li,num,j) if (j==li.length) => result
      //case (li,num,j) =>

    //}
    //var results = List[List[Any]]()
    //for (i<-0 to l.length-1) results=generateCombinationsR(List[Any](),l.remove(i)._1,n,i)::results
  //}


  def sortList(l: List[List[Any]]): List[List[Any]] = {
    l.sort((l1:List[Any], l2: List[Any]) => l1.length<l2.length)
  }

  def sortListBis(l:List[List[List[Any]]]):List[List[List[Any]]] = l.sort((l1:List[List[Any]], l2: List[List[Any]]) => l1.length<l2.length)

  def flattenAgainBis(l: List[List[List[Any]]]): List[List[Any]] = l match{
    case Nil => Nil
    case (h::tail) if (h.length==0) => flattenAgainBis(tail)
    case (h::tail) => h.head::flattenAgainBis(h.tail::tail)
  }

  def sortFreq (l: List[List[Any]]): List[List[Any]] = {
    val l1 = sortList(l)

    def sortFreqR(result: List[List[List[Any]]], curList: List[List[Any]], bufferList: List[List[Any]],buffer: Int):List[List[List[Any]]] = (curList, bufferList, buffer) match {
      case (Nil,buf,_) => result:::List(buf)
      case (h::tail,_,-1) => sortFreqR(result, tail,List(h),h.length)
      case (h::tail,bufL,buf) if(h.length==buf) => sortFreqR(result,tail,h::bufL, buf)
      case (h::tail,bufL,buf) => sortFreqR(result:::List(bufL),tail,List(h),h.length)
    }
    var resultI = sortFreqR(List[List[List[Any]]](),l1,List[List[Any]](),-1)
    var resultII = sortListBis(resultI)
    flattenAgainBis(resultII)
  }
}
