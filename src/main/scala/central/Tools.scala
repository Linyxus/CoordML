package central

object Tools {
  // duplicate x for n times
  def duplicate[A](x: List[A], n: Int): List[A] =
    if (n <= 0) Nil
    else x ++ duplicate(x, n - 1)

  // average maybe list
  def maybeAverage(x: List[Option[Double]]): Double =
    (x map { m => m getOrElse 0.0 }).sum / x.count(p => p.isDefined).toDouble

  def gatherOption[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case x :: rem =>
      for (z <- x; zs <- gatherOption(rem)) yield z :: zs
    case Nil => Some(Nil)
  }
}
