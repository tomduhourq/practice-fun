val firstNumbers = List(3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8)
val tens = List(3, 6, 6, 5, 5, 5, 7, 6, 6)

lazy val sumToString: Int => Int = {
  case n if(n < 20) => firstNumbers(n - 1)
  case n if(n < 100) =>
    tens(n / 10 - 1) + (if(n % 10 > 0) firstNumbers(n % 10 - 1) else 0)
  case n if(n < 1000) =>
    sumToString(n / 100) + 7 + (if(n % 100 > 0) 3 + sumToString(n % 100) else 0)
  case 1000 => 11
}

(1 to 1000).map(sumToString).sum // 21124