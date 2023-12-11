#' @title Deduplicate DQA (Data Quality Assessment) data.
#'
#' @description This function identifies and removes duplicate records in the DQA data based on specified criteria.
#'
#' @param df A dataframe containing DQA data.
#' @return A deduplicated dataframe.
#'
#' @examples
#' # Create sample dataframe
#' df <- data.frame(
#'   facility = c("A", "A", "B", "B", "C", "C"),
#'   client_id = c(1, 2, 1, 3, 4, 2),
#'   abstracted_days_of_arv_refill = c(30, 28, 30, 28, 30, 28),
#'   abstracted_art_start_date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", NA, NA)),
#'   abstracted_last_pickup_date = as.Date(c("2022-01-28", "2022-02-28", "2022-03-28", "2022-04-28", "2022-05-28", "2022-06-28"))
#' )
#'
#' # Deduplicate DQA data
#' result <- deduplicate_dqa(df)
#'
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#'
#' @export


deduplicate_dqa <- function(df) {
  # Step 1: Identify duplicates based on facility and client_id
  dups <- df |>
    get_dupes(
      facility, client_id
    )
  # Step 2: Identify perfect duplicates based on specified columns
  perfect_dupes <- dups |>
    get_dupes(
      abstracted_days_of_arv_refill,
      abstracted_art_start_date,
      abstracted_last_pickup_date,
      abstracted_days_of_arv_refill
    )
  # Step 3: Identify partial duplicates by excluding perfect duplicates
  partial_dupes <- dups |>
    filter(
      !client_id %in% perfect_dupes$client_id
    )

  # Step 4: Create the final deduplicated dataframe
  final_df <- df |>
    filter(
      !client_id %in% dups$client_id,
      facility %in% dups$facility
    ) |>
    bind_rows(
      perfect_dupes |>
        distinct(facility, client_id, .keep_all = TRUE)
    )

  return(final_df)

}
