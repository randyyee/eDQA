#' @title Get non-concurring records between cleaned and extracted data.
#'
#' @description This function identifies records that do not match between cleaned and extracted data based on specified criteria.
#'
#' @param df A dataframe containing cleaned and extracted data.
#' @return A dataframe with non-concurring records.
#'
#' @examples
#' # Create sample dataframe
#' df <- data.frame(
#'   facility = c("A", "A", "B", "B", "C", "C"),
#'   sex = c("M", "F", "M", "F", "M", "F"),
#'   art_start_date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", NA, NA)),
#'   last_pickup_date = as.Date(c("2022-01-28", "2022-02-28", "2022-03-28", "2022-04-28", "2022-05-28", "2022-06-28")),
#'   days_of_arv_refill = c(30, 28, 30, 28, 30, 28),
#'   abstracted_sex = c("M", "F", "M", "F", "M", "F"),
#'   abstracted_art_start_date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01")),
#'   abstracted_last_pickup_date = as.Date(c("2022-01-28", "2022-02-28", "2022-03-28", "2022-04-28", "2022-05-28", "2022-06-28")),
#'   abstracted_days_of_arv_refill = c(30, 28, 30, 28, 30, 28)
#' )
#'
#' # Get non-concurring records
#' result <- get_non_concurrence(df)
#'
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#'
#' @export


get_non_concurrence <- function(df) {
  # Step 1: Filter records with corresponding entries on the EMR
  # Step 2: Calculate match indicators for specified columns
  # Step 3: Filter non-concurring records based on match indicators
  df |>
    filter(
      !is.na(art_start_date),!is.na(last_pickup_date),!is.na(sex),!is.na(days_of_arv_refill)
    ) |>
    mutate(
      sex_match = if_else(sex == abstracted_sex,
                          TRUE,
                          FALSE,
                          NA),
      art_start_match = if_else(art_start_date == abstracted_art_start_date,
                                TRUE,
                                FALSE,
                                NA),
      pickup_match = if_else(
        last_pickup_date == abstracted_last_pickup_date,
        TRUE,
        FALSE,
        NA
      ),
      days_of_refill_match = if_else(
        days_of_arv_refill == abstracted_days_of_arv_refill,
        TRUE,
        FALSE,
        NA
      )
    ) |>
    filter(
      sex_match == FALSE |
        art_start_match == FALSE |
        pickup_match == FALSE |
        days_of_refill_match == FALSE
    )

}
