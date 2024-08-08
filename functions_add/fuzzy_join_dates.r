fuzzy_join_dates <- function(extraction_date, release_date, n = 1, len_hist = 18) {
  # Finds the n release dates immediately prior to the extraction date.
  #
  # Args:
  #   extraction_date: The extraction_date column from a dataframe.
  #   release_date:    The release_date column from a dataframe.
  #   n:               The number of prior periods to find. Defaults to 1.
  #   len_hist:        The length of history to use in months. Ensures that the matching release dates are not too old. Defaults to 18 months.
  #
  # Returns:
  #   A dataframe that enables fuzzy date join.

  # Find the corresponding dates to join on
  crossing(extraction_date, release_date) %>%
    filter(
      # Release must be prior to extraction
      extraction_date >= release_date,
      # Make sure that the matching release dates are not too old
      (extraction_date - release_date) <= months(len_hist, abbreviate = FALSE)
    ) %>%
    # Find the n matching dates
    group_by(extraction_date) %>%
    slice_max(release_date, n = n) %>%
    mutate(prior_period = row_number()) %>%
    ungroup()
}