#' @title Validate TX_CURR
#' @description TBD
#' @param df A dataframe.


validate_tx_curr <- function(df, .by = "assessor") {
  if (.by == "assessor") {
    df |>
      mutate(
        date_lost = abstracted_last_pickup_date +
          abstracted_days_of_arv_refill + 28,
        status = if_else(
          date_lost < ymd("2023-06-30"),
          "Inactive",
          "Active",
          "Not Documented"
        )
      ) |>
      filter(status == "Active") |>
      count(
        facility,
        submitted_by,
        name = "active",
        sort = TRUE,
        .drop = FALSE
      ) |>
      left_join(
        df |>
          mutate(
            date_lost = abstracted_last_pickup_date +
              abstracted_days_of_arv_refill + 28,
            status = if_else(
              date_lost < ymd("2023-06-30"),
              "Inactive",
              "Active",
              "Not Documented"
            )
          ) |>
          count(facility, submitted_by, name = "abstracted_records", .drop = FALSE),
        join_by(facility, submitted_by)
      ) |>
      adorn_totals() |>
      mutate(percent_validated = round_half_up(active / abstracted_records, digits = 3)) |>
      arrange(facility, desc(percent_validated))

  } else if (.by == "day") {
    df |>
      mutate(
        date_lost = abstracted_last_pickup_date + abstracted_days_of_arv_refill + 28,
        status = if_else(
          date_lost < ymd("2023-06-30"),
          "Inactive",
          "Active",
          "Not Documented"
        )
      ) |>
      filter(status == "Active") |>
      count(facility,
            date_of_assessment,
            name = "active",
            .drop = FALSE) |>
      left_join(
        df |>
          mutate(
            date_lost = abstracted_last_pickup_date +
              abstracted_days_of_arv_refill + 28,
            status = if_else(
              date_lost < ymd("2023-06-30"),
              "Inactive",
              "Active",
              "Not Documented"
            )
          ) |>
          count(
            facility,
            date_of_assessment,
            name = "abstracted_records",
            .drop = FALSE
          ),
        join_by(facility, date_of_assessment)
      ) |>
      adorn_totals() |>
      mutate(percent_validated = round_half_up(active / abstracted_records, digits = 3)) |>
      arrange(facility, date_of_assessment)

  } else if (.by == "assessor_daily") {
    df |>
      mutate(
        date_lost = abstracted_last_pickup_date + abstracted_days_of_arv_refill + 28,
        status = if_else(
          date_lost < ymd("2023-06-30"),
          "Inactive",
          "Active",
          "Not Documented"
        )
      ) |>
      filter(status == "Active") |>
      count(facility,
            date_of_assessment,
            submitted_by,
            name = "active",
            .drop = FALSE) |>
      left_join(
        df |>
          mutate(
            date_lost = abstracted_last_pickup_date +
              abstracted_days_of_arv_refill + 28,
            status = if_else(
              date_lost < ymd("2023-06-30"),
              "Inactive",
              "Active",
              "Not Documented"
            )
          ) |>
          count(
            facility,
            date_of_assessment,
            submitted_by,
            name = "abstracted_records",
            .drop = FALSE
          ),
        join_by(facility, date_of_assessment, submitted_by)
      ) |>
      adorn_totals() |>
      mutate(percent_validated = round_half_up(active / abstracted_records, digits = 3)) |>
      arrange(facility, date_of_assessment)

  } else if (.by == "site") {
    df |>
      mutate(
        date_lost = abstracted_last_pickup_date +
          abstracted_days_of_arv_refill + 28,
        status = if_else(
          date_lost < ymd("2023-06-30"),
          "Inactive",
          "Active",
          "Not Documented"
        )
      ) |>
      filter(status == "Active") |>
      count(facility, name = "active", .drop = FALSE) |>
      left_join(
        df |>
          mutate(
            date_lost = abstracted_last_pickup_date +
              abstracted_days_of_arv_refill + 28,
            status = if_else(
              date_lost < ymd("2023-06-30"),
              "Inactive",
              "Active",
              "Not Documented"
            )
          ) |>
          count(facility, name = "abstracted_records", .drop = FALSE),
        join_by(facility)
      ) |>
      adorn_totals() |>
      mutate(percent_validated = round_half_up(active / abstracted_records, digits = 3)) |>
      arrange(desc(percent_validated))
  }
}
