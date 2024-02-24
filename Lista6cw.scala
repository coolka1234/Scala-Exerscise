@main
def main(): Unit = {
  println("Hello world!")
  println(lrepeat(3,LazyList(1,5,6,7)).toList)
  println(lfib.take(15).toList)
  println(lBreadth(lTree(5)).take(7).toList)
}
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]
def lrepeat[T](n: Int, s: LazyList[T]): LazyList[T] = {
  def lrep(n: Int, x: T, streamRest: () => LazyList[T]): LazyList[T] = {
    if (n > 1) {
      x #:: lrep(n - 1, x, streamRest)
    } else {
      x #:: streamRest()
    }
  }

  if (s.isEmpty) {
    LazyList()
  } else {
    val x #:: xs = s //zamieniamy liste na ogon i glowe
    lrep(n, x, () => lrepeat(n, xs)) //powtarzamy glowe x n razy i wywolujemy dla ogona listy
  }
}
val lfib = {
  def lfibIn(p: Int, n: Int): LazyList[Int] = {
    (p + n) #:: lfibIn(n, (p + n))
  }

  LazyList.cons(0, LazyList.cons(1, lfibIn(0, 1)))
}
def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}
def lBreadth[T](ltree: lBT[T]): LazyList[T] = {
  def bfsIn(queue: List[lBT[T]]): LazyList[T] = {
    queue match {
      case Nil => LazyList()
      case LEmpty :: t => bfsIn(t)
      case LNode(v, l, r) :: t => LazyList.cons(v, bfsIn(t ++ List(l(), r()))) //dodaj lewe o prawe do dalszej eksploracji
    }
  }

  bfsIn(List(ltree))
}