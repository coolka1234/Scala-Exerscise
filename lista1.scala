object App1{
  def flatten[T](list: List[List[T]]): List[T] = {

    if (list != null && list.nonEmpty) {
      list.head ++ flatten(list.tail)
    } else Nil
  }
}