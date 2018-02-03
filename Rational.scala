package scala_01

class Rational(n:Int,d:Int) {

  /**
    * @ author=zxq
    * desc="主要是建立有理数之间的运算
    * 其中包括加减乘除运算和比较大小
    *
    *
    */
  require(d!=0)
  private val g=gcd(n.abs,d.abs)
  val number=n/g
  val denom=d/g

//将有理数化简成最简化转化成字符串输出
  override def toString: String = {
    val NewR=new Rational(this.number,this.denom)
    if (NewR.denom==1)
      return NewR.number+""
    return NewR.number+"/"+NewR.denom
  }
//有理数的加法运算
  def add(that:Rational):Rational= {
      new Rational(number * that.denom + that.number * denom, denom * that.denom)
    }
  def +(that:Rational):Rational={
    new Rational(number * that.denom + that.number * denom, denom * that.denom)

  }
  //有理数和整数之间的加法运算
  def add(i:Int):Rational={
    new Rational(this.number+this.denom*i,this.denom)
  }
  def +(i:Int):Rational={
    new Rational(this.number+this.denom*i,this.denom)
  }
//有理数之间比较大小
  def lessThan(that:Rational)={
    this.number*that.denom<that.number*this.denom
    }
  def lessThan(i:Int)={
    this.number<i*this.denom
  }
//获取两个有理数中的最大数
  def max(that:Rational)={
      if(this.lessThan(that)) that else this
    }
  def max(i :Int)={
    if(this.lessThan(i)) i else this
  }

  //获取两个有理数中的最小数
  def min(that:Rational)={
    if(this.lessThan(that)) this else that
  }
  def min(i:Int)={
    if(this.lessThan(i)) this else i
  }
  //两个有理数的乘法运算
  def multip(that:Rational)={
    new Rational(that.number*this.number,this.denom*that.denom)
  }
  def *(that:Rational)={
    new Rational(that.number*this.number,this.denom*that.denom)

  }
  def multip(i:Int)={
    new Rational(this.number*i,this.denom)
  }
  def *(i:Int)={
    new Rational(this.number*i,this.denom)
  }


  def this(n:Int)=this(n,1)//辅助构造器
  //两个有理数的减法运算
  def minus(that:Rational)={
    new Rational(this.number*that.denom-this.denom*that.number,this.denom*that.denom)
  }
  def -(that:Rational)={
    new Rational(this.number*that.denom-this.denom*that.number,this.denom*that.denom)
  }
  def minus(i :Int)={
    new Rational(this.number-i*this.denom,this.denom)
  }
  def -(i:Int)={
    new Rational(this.number-i*this.denom,this.denom)
  }
  //有理数的最简化
  private def gcd(a: Int, b: Int):Int={
    if (b==0) a else gcd(b,a%b)
  }
  //有理数之间的除法运算
  def division(that:Rational)={
    new Rational(this.number*that.denom,that.number*this.denom)
  }
  def /(that:Rational)={
    new Rational(this.number*that.denom,that.number*this.denom)
  }
  def division(i:Int)={
    new Rational(this.number,this.denom*i)
  }
  def /(i:Int)={
    new Rational(this.number,this.denom*i)
  }






}
