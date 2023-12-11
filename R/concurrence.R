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


check_concurrence <-
  function(df_cleaned, df_extracted, .by = "assessor") {
    if (.by == "assessor") {
      ### pull only records that have corresponding entries on the EMR (i.e. they have been reported as active in Q3)
      df_cleaned |>
        filter(
          !is.na(art_start_date),!is.na(last_pickup_date),!is.na(sex),!is.na(days_of_arv_refill)
        ) |>
        mutate(
          sex_match = if_else(sex == abstracted_sex,
                              TRUE,
                              FALSE,
                              NA),
          art_start_match = if_else(
            art_start_date == abstracted_art_start_date,
            TRUE,
            FALSE,
            NA
          ),
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
        summarise(
          records_reported_active = n(),
          across(ends_with("match"), \(x) sum(x, na.rm = TRUE)),
          .by = c(facility, submitted_by)
        ) |>
        left_join(
          df_extracted |>
            count(facility, submitted_by, name = "entries_submitted", .drop = FALSE),
          join_by(facility, submitted_by)
        ) |>
        relocate(entries_submitted, .after = submitted_by) |>
        adorn_totals() |>
        mutate(
          percent_sex = round_half_up(sex_match / records_reported_active, digits = 3),
          percent_art_start = round_half_up(art_start_match / records_reported_active, digits = 3),
          percent_pickup = round_half_up(pickup_match / records_reported_active, digits = 3),
          percent_refill = round_half_up(days_of_refill_match / records_reported_active, digits = 3)
        )

    } else if (.by == "day") {
      ### pull only records that have corresponding entries on the EMR (i.e. they have been reported as active in Q3)
      df_cleaned |>
        filter(
          !is.na(art_start_date),!is.na(last_pickup_date),!is.na(sex),!is.na(days_of_arv_refill)
        ) |>
        mutate(
          sex_match = if_else(sex == abstracted_sex,
                              TRUE,
                              FALSE,
                              NA),
          art_start_match = if_else(
            art_start_date == abstracted_art_start_date,
            TRUE,
            FALSE,
            NA
          ),
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
        summarise(
          records_reported_active = n(),
          across(ends_with("match"), \(x) sum(x, na.rm = TRUE)),
          .by = c(facility, date_of_assessment)
        ) |>
        left_join(
          df_extracted |>
            count(
              facility,
              date_of_assessment,
              name = "entries_submitted",
              .drop = FALSE
            ),
          join_by(facility, date_of_assessment)
        ) |>
        relocate(entries_submitted, .after = date_of_assessment) |>
        adorn_totals() |>
        mutate(
          percent_sex = round_half_up(sex_match / records_reported_active, digits = 3),
          percent_art_start = round_half_up(art_start_match / records_reported_active, digits = 3),
          percent_pickup = round_half_up(pickup_match / records_reported_active, digits = 3),
          percent_refill = round_half_up(days_of_refill_match / records_reported_active, digits = 3)
        )

    } else if (.by == "site") {
      ### pull only records that have corresponding entries on the EMR (i.e. they have been reported as active in Q3)
      df_cleaned |>
        filter(
          !is.na(art_start_date),!is.na(last_pickup_date),!is.na(sex),!is.na(days_of_arv_refill)
        ) |>
        mutate(
          sex_match = if_else(sex == abstracted_sex,
                              TRUE,
                              FALSE,
                              NA),
          art_start_match = if_else(
            art_start_date == abstracted_art_start_date,
            TRUE,
            FALSE,
            NA
          ),
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
        summarise(
          records_reported_active = n(),
          across(ends_with("match"), \(x) sum(x, na.rm = TRUE)),
          .by = c(facility)
        ) |>
        left_join(
          df_extracted |>
            count(facility, name = "entries_submitted", .drop = FALSE),
          join_by(facility)
        ) |>
        relocate(entries_submitted, .after = facility) |>
        adorn_totals() |>
        mutate(
          percent_sex = round_half_up(sex_match / records_reported_active, digits = 3),
          percent_art_start = round_half_up(art_start_match / records_reported_active, digits = 3),
          percent_pickup = round_half_up(pickup_match / records_reported_active, digits = 3),
          percent_refill = round_half_up(days_of_refill_match / records_reported_active, digits = 3),
        )
    }
    return(df_cleaned)
  }
