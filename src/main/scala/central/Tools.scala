package central

object Tools {
  def duplicate[A](x: List[A], n: Int): List[A] =
    if (n <= 0) Nil
    else x ++ duplicate(x, n - 1)
}
