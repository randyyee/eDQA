#' @title Exclude inactive records from the dataframe.
#'
#' @description This function performs the following steps:
#'
#' 1. Calculates the date when a client is considered lost based on the last pickup date and days of ARV refill.
#' 2. Identifies records that are not eligible (inactive) based on the calculated date_lost and missing values in key columns.
#' 3. Excludes the not eligible records from the original dataframe, resulting in a dataframe of eligible records.
#'
#' @param df A dataframe containing the data to be processed.
#' @return A dataframe excluding inactive records.
#'
#' @examples
#' # Create a sample dataframe
#' df <- data.frame(
#'   client_id = c(1, 2, 3, 4, 5),
#'   abstracted_last_pickup_date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01")),
#'   abstracted_days_of_arv_refill = c(30, 28, 30, 28, 30),
#'   art_start_date = as.Date(c("2021-12-01", "2022-01-01", NA, "2022-04-01", "2022-05-01")),
#'   last_pickup_date = as.Date(c("2022-01-28", "2022-02-28", "2022-03-28", "2022-04-28", "2022-05-28")),
#'   days_of_arv_refill = c(30, 28, 30, NA, 30)
#' )
#'
#' # Exclude inactive records
#' result <- exclude_inactive(df)
#'
#' @import dplyr
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr is.na
#' @export


exclude_inactive <- function(df) {
  # Step 1: Calculate the date when a client is considered lost
  not_eligible <- df |>
    mutate(date_lost = abstracted_last_pickup_date + abstracted_days_of_arv_refill + 28) |>
    # Step 2: Identify records that are not eligible
    filter(
      date_lost <= ymd("2023-06-30"),
      is.na(art_start_date),
      is.na(last_pickup_date),
      is.na(days_of_arv_refill)
    )

  # Step 3: Exclude not eligible records from the original dataframe
  eligible <- df |>
    filter(!client_id %in% not_eligible$client_id)

  return(eligible)
}
