// Draw n different numbers from 1 to M
def lotto(n: Int, m: Int) =
  (1 to n)
    .foldLeft(List.empty: List[Int])((l, _) => l ::: List(scala.util.Random.nextInt(m + 1) + 1))

lotto(6, 49)