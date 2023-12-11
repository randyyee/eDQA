#' @title Calculate site summary statistics for a dataframe.
#' @description TBD
#' @param df A dataframe.

site_summary <- function(df) {
  entries <- df |> count(facility, name = "no_of_entries")

  summary <- df |>
    count(facility, submitted_by, name = "no_of_entries") |>
    summarise(
      mean_entry = mean(no_of_entries, na.rm = TRUE),
      sd_entry =  sd(no_of_entries, na.rm = TRUE),
      min_entry = min(no_of_entries, na.rm = TRUE),
      median_entry = median(no_of_entries, na.rm = TRUE),
      max_entry = max(no_of_entries, na.rm = TRUE),
      iqr_entry = IQR(no_of_entries, na.rm = TRUE),
      .by = facility
    )

  entries |>
    left_join(summary,
              join_by(facility))

}
