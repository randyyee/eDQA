#' @title Get discounted duplicates from the dataframe.
#'
#' @description This function performs the following steps:
#'
#' 1. Identify duplicates based on facility and client_id and remove dupe_count column.
#' 2. Identify perfect duplicates based on specific columns and remove dupe_count column.
#' 3. Identify repeated perfect duplicates and select the second occurrence.
#' 4. Identify partial duplicates by excluding perfect duplicates based on specific columns.
#' 5. Combine repeated perfect duplicates and partial duplicates into a single dataframe.
#'
#' @param df A dataframe containing the data to be processed.
#' @return A dataframe containing discounted duplicates.
#'
#' @examples
#' # Create a sample dataframe
#' df <- data.frame(
#'   facility = c("A", "A", "B", "B", "C", "C"),
#'   client_id = c(1, 1, 2, 3, 4, 4),
#'   state = c("NY", "NY", "CA", "TX", "FL", "FL"),
#'   date_of_assessment = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01")),
#'   sex = c("M", "M", "F", "M", "F", "F"),
#'   art_start_date = as.Date(c("2021-12-01", "2021-12-01", "2022-01-01", "2022-01-01", NA, NA)),
#'   last_pickup_date = as.Date(c("2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28")),
#'   days_of_arv_refill = c(30, 30, 28, 28, 30, 30),
#'   abstracted_sex = c("M", "M", "F", "M", "F", "F"),
#'   abstracted_art_start_date = as.Date(c("2021-12-01", "2021-12-01", "2022-01-01", "2022-01-01", NA, NA)),
#'   abstracted_last_pickup_date = as.Date(c("2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28", "2022-01-28")),
#'   abstracted_days_of_arv_refill = c(30, 30, 28, 28, 30, 30),
#'   submitted_by = c("X", "X", "Y", "Z", "W", "W")
#' )
#'
#' # Get discounted duplicates
#' result <- get_discounted_duplicates(df)
#'
#' @import dplyr
#' @importFrom dplyr slice
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr join_by
#' @importFrom dplyr get_dupes
#'
#' @export


get_discounted_duplicates <- function(df) {
  dups <- df |>
    get_dupes(facility, client_id) |>
    select(-dupe_count)

  perfect_dupes <- dups |>
    get_dupes(
      abstracted_days_of_arv_refill,
      abstracted_art_start_date,
      abstracted_last_pickup_date,
      abstracted_days_of_arv_refill
    ) |>
    select(-dupe_count)

  perfect_dupes_repeat <- perfect_dupes |>
    slice(-1,
          .by = c(facility, client_id))

  partial_dupes <- dups |>
    anti_join(
      perfect_dupes,
      join_by(
        facility,
        client_id,
        state,
        date_of_assessment,
        sex,
        art_start_date,
        last_pickup_date,
        days_of_arv_refill,
        abstracted_sex,
        abstracted_art_start_date,
        abstracted_last_pickup_date,
        abstracted_days_of_arv_refill,
        submitted_by
      )
    )

  total_dupes <- perfect_dupes_repeat |>
    bind_rows(partial_dupes)

  return(total_dupes)

}
