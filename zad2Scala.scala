@main
def main(): Unit = {
  println(listLength(List(1,2,3,4,5)));
  println(listLength1(List(1,2,3,List(1,3,4),5,6)));
  println(listLength1(List(0)));
}
def listLength[A](xs: List[A]): Int = xs match {
  case Nil => 0
  case _ :: tail => 1 + listLength(tail)
}
def listLength1[A](xs: List[A]): Int = {
    if(xs.head==Nil) 0
    else listLength(xs.tail)+1;
}
