#' @title Calculate daily summary statistics for a dataframe.
#'
#' @description This function performs the following steps:
#'
#' 1. Counts the number of assessors per facility and date_of_assessment.
#' 2. Counts the number of unique entries per facility and date_of_assessment.
#' 3. Calculates summary statistics (mean, standard deviation, min, median, max) for unique entries
#'    per facility and date_of_assessment.
#' 4. Combines assessors, entries, and summary into a final summary dataframe.
#'
#' @param df A dataframe containing the data to be summarized.
#' @return A dataframe with daily summary statistics.
#'
#' @examples
#' # Create a sample dataframe
#' df <- data.frame(
#'   facility = c("A", "A", "B", "B", "C"),
#'   date_of_assessment = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02")),
#'   submitted_by = c("X", "Y", "X", "Y", "Z")
#' )
#'
#' # Calculate daily summary
#' result <- daily_summary(df)
#'
#' @import dplyr
#' @importFrom tidyr distinct
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom dplyr summarise
#' @importFrom dplyr count
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#'
#' @export

# Optimized daily_summary function with comments
daily_summary <- function(df) {
  # Step 1: Count the number of assessors per facility and date_of_assessment
  assessors <- df |>
    distinct(facility, date_of_assessment, submitted_by, .keep_all = TRUE) |>
    count(facility, date_of_assessment, name = "no_of_assessors")

  # Step 2: Count the number of unique entries per facility and date_of_assessment
  entries <- df |>
    count(facility, date_of_assessment, name = "unique_entries")

  # Step 3: Calculate summary statistics for unique_entries per facility and date_of_assessment
  summary <- df |>
    count(facility, date_of_assessment, submitted_by, name = "unique_entries") |>
    summarise(
      mean_entry = mean(unique_entries, na.rm = TRUE),
      sd_entry = sd(unique_entries, na.rm = TRUE),
      min_entry = min(unique_entries, na.rm = TRUE),
      median_entry = median(unique_entries, na.rm = TRUE),
      max_entry = max(unique_entries, na.rm = TRUE),
      .by = c(facility, date_of_assessment)
    )

  # Step 4: Combine assessors, entries, and summary into a final summary data frame
  final_summary <- assessors |>
    left_join(entries, by = c("facility", "date_of_assessment")) |>
    left_join(summary, by = c("facility", "date_of_assessment"))

  return(final_summary)
}
