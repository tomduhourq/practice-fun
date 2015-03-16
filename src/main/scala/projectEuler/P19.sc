trait Day {
  def next : Day
}

object Monday extends Day {
  def next = Tuesday
}

object Tuesday extends Day {
  def next = Wednesday
}

object Wednesday extends Day {
  def next = Thursday
}

object Thursday extends Day {
  def next = Friday
}

object Friday extends Day {
  def next = Saturday
}

object Saturday extends Day {
  def next = Sunday
}

object Sunday extends Day {
  def next = Monday
}

