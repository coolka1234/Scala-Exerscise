import scala.annotation.tailrec

sealed trait BT[A]
case class Empty[A]() extends BT[A]
case class Node[A](elem:A, left:BT[A], right:BT[A]) extends BT[A]
val t1 = Node(1,
  Node(2,
    Node(4,
      Empty(),
      Empty()
    ),
    Empty()
  ),
  Node(3,
    Node(5,
      Empty(),
      Node(6,
        Empty(),
        Empty()
      )
    ),
    Empty()
  )
)
val t2 = Node(10,
  Node(10,
    Empty(),
    Empty()
  ),
  Node(10,
    Node(10,
      Empty(),
      Empty()
    ),
    Node(16,
      Empty(),
      Empty()
    )
  )
)
val t3 = Node(15,
  Node(2,
    Node(41,
      Empty(),
      Empty()
    ),
    Empty()
  ),
  Node(39,
    Node(3,
      Empty(),
      Node(666,
        Empty(),
        Empty()
      )
    ),
    Empty()
  )
)
val t4 = Empty()

def tree_my(node: BT[Int]): Int = {

  @tailrec def mult_with_stack(stack: List[BT[Int]], result: Int): Int = {
    stack match {
      case Nil => result
      case Empty() :: rest => mult_with_stack(rest, result) //dla pustych
      case Node(v, l, r) :: rest =>
        val stosNowy = l :: r :: rest
        val wynikNowy = result * v
        mult_with_stack(stosNowy, wynikNowy)
    }
  }

  mult_with_stack(List(node), 1)
}
@main
def main(): Unit = {
  println(tree_my(t1))
  println(tree_my(t2))
  println(tree_my(t3))
}