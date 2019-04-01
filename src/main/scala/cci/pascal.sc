

//            1
//          1   1
//        1   2   1
//      1   3   3   1
//    1   4   6   4   1
//  1   5   10  10  5   1

// pascal(5, 1) == 5
// pascal(0, 0) == 1
// pascal(4, 3) == 4
// y nunca salir de ahi

def pascal(c: Int, r: Int): Int =
  if (c == 0 || c == r) 1
  else pascal(c-1, r-1) + pascal(c, r-1)


def pascal2(r: Int, c: Int): Int =
  if(c == 0 || c == r) 1
  else pascal2(r-1, c-1) + pascal2(r-1, c)


pascal(1, 5)
pascal(0, 0)
pascal(3, 4)


pascal2(5, 1)
pascal2(0, 0)
pascal2(4, 3)