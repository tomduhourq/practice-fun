BigInt(2)
  .pow(1000)
  .toString
  .toList
  // mapping toInt returns the ascii rep, so if we subtract 48
  .map(_.toInt - 48)
  .sum