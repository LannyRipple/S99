package me.rémi.s99.arithmetic

object S199Int {

  implicit def int2S199Int(i: Int): S199Int = new S199Int(i)

  def main(args: Array[String]) {
    printGoldbachListLimited(1,3000,50)
  }

  def gcd(a: S199Int, b: S199Int): S199Int ={
    if (a.start%b.start==0) b.start
    else gcd(b.start,a.start%b.start)
  }

  def goldbach(n: Int):(Int,Int) = {
    var p = 2
    while(!isPrime(n-p)) p = nextPrime(p)
    (p,n-p)
  }

  def printGoldbachList(j:Int,k:Int) {
    var i=j
    while(i<=k){
      if (i%2==0){
        val gb = goldbach(i)
        println(i+" = "+gb._1+" + "+gb._2)
      }
      i+=1
    }
  }

  def printGoldbachListLimited(j:Int,k:Int,n:Int) {
    var i=j
    while(i<=k){
      val gb = goldbach(i)
      if (i%2==0&&gb._1>n){
        println(i+" = "+gb._1+" + "+gb._2)
      }
      i+=1
    }
  }

  def nextPrime(n: Int): Int = {
    require (isPrime(n))
    if (n==2) 3
    else{
	var p = n+2
	while (!isPrime(p)) p+=1
	p
    }
  }

  def isPrime(n: Int): Boolean = {
    if (n<11) List(2,3,5,7).contains(n)
    else{
      val r = Math.sqrt(n.toDouble).toInt
      var d = 5
      while (d<=r){
        if (n%d == 0) return false
        else{
          if (n%(d+2) == 0) return false
        }
        d+=6
      }
      true
    }
  }
}

class S199Int(val start: Int) {
    import S199Int.{isPrime => isPrimeF, _}

  def totient: Int = {
    var result = 0
    for (i<-1 to start){
      if (isCoPrime(i)) result+=1
    }
    result
  }

  def isCoPrime(n: Int): Boolean = {
    (gcd(start,n)==1)
  }

  lazy val isPrime: Boolean = isPrimeF(start)
}
