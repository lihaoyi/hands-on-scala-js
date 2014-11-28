trait T{
  s match { case S(_, r @ _*) => 1 }
}