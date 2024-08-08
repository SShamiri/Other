nth_weekday_in_mth <- function(yr, mth, n, wkday) {
  # Finds the nth occurence of a particular weekday in a given month.
  #
  # Args:
  #   yr:    The year to use. E.g. "2020"
  #   mth:   The month to use as an integer. E.g. "10" for October
  #   n:     The occurence to return. E.g. "1" for the first occurence, "-2" for the second last occurence.
  #   wkday: The day of the week in abbreviated 3 letter form. E.g. "Fri" for Friday.
  #
  # Returns:
  #   The date that is the nth occurence of the weekday in the given month and year.

  stopifnot(
    # Enforce types for arguments
    is.numeric(yr),
    is.numeric(mth),
    is.numeric(n),
    is.character(wkday),
    # Enforce values for arguments
    yr > 0,
    between(mth, 1, 12),
    between(n, -5, 5),
    wkday %in% c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  )
  
  # Create a series of every day in the month
  dates <- seq(from = as.Date(paste(yr, mth, "01", sep = "-")),
               to = as.Date(paste(yr, mth, days_in_month(mth), sep = "-")),
               by = 1)

  # Search for the days of the week
  df <- data.frame(date = dates, weekday = wday(dates, label = TRUE)) %>%
    filter(weekday == wkday)
  
  # Reverse order for counting backwards
  if (n < 0) {
    df <- df %>%
      arrange(desc(date))
  }
  
  # Find the occurence  
    df %>%
      filter(row_number() == abs(n)) %>%
      pull(date)
  
}