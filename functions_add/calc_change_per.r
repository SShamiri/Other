calc_change_per <- function(column, len, dt) {
  # Calculates the percentage change of a column in a dataframe over a specified amount of time.
  #
  # Args:
  #   column: The column to use for calculating the change.
  #   len:    The number of lags to use
  #   dt:     The date column to order the column values by.
  #
  # Returns:
  #   A vector containing the change values.

  prev_val <- lag(column, n = len, order_by = dt)
  change <- ((column - prev_val)/prev_val)

  # Special case when current and previous value are both 0
  change[is.nan(change)] <- 0

  change
}