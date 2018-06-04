sealed trait A
case class A1(i: Int) extends A
case class A2(d: Double) extends A
case class A3(i: Int, d: Double) extends A
class A4 extends A

def matcher(a: A): String = a match {
  case _: A1 | _: A2 => "single"
  case _: A3 => "pair"
  case _: A4 => "none"
}

matcher(A1(1))
