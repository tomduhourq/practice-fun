val lengths = List(31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

// map days of months
val months = for{y <- 1900 to 2000; m <- 1 to 12}
  yield
    if(m == 2)
      if (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) 29 else 28
    else
      lengths(m - 1)

// I want the intermediate results
val fs = months.scanLeft(1)((ws, l) => (ws + l) % 7)

// drop first year
fs.drop(12).take(1200).count(_ == 0)

