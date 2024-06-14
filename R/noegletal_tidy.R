#' Transform a csv file from noegletal.dk into a tidy tibble
#'
#' `noegletal_tidy` takes a CSV-file downloaded from noegletal.dk as input and parses it into a tidy `tibble`.
#' This `tibble` has one row for each municipality-year, a `muni_code` column, a `year` column and a column for each `variable` (as selected on noegletal.dk).
#' As per the noegletal.dk documentation, cells with a dash `-` as a value is converted to a 0, while cells with a value of `M` or `U` is converted to `NA`, since these represent missing values.
#'
#' @param file Path to a csv file downloaded from noegletal.dk.
#'
#' @returns A tidy `tibble` with one row for each municipality-year, and one column for each included variable (nøgletal).
#'
#' @export
noegletal_tidy <- function(file) {
  data <- readr::read_delim(file,
                            delim=";",
                            locale = readr::locale(encoding = "latin1"),
                            skip = 2) |>
    dplyr::rename(variable = `...1`, muni_code = Kom.nr) |>
    dplyr::filter(!stringr::str_ends(variable, "Kommune") |
                    !rowSums(is.na(dplyr::across(dplyr::everything()))) > 1)

  # Insert the correct `variable` in the variable col.
  for (i in seq_len(nrow(data))) {
    if (i > 1 && stringr::str_ends(data$variable[i], "Kommune")) {
      data$variable[i] <- data$variable[i - 1]
    }
  }

  # Slet rækker der ikke indeholder muni_code (fjerner fodnoter samt variabelrækker)
  data <- data |>
    dplyr::filter(!is.na(muni_code))

  # Pivot to long format
  data <- data |>
    tidyr::pivot_longer(cols = 3:ncol(data), names_to = "year")

  # Clean up values
  data <- data |>
    dplyr::mutate(value = stringr::str_replace_all(value, "\\.", ""), # remove thousand separator
                  value = stringr::str_replace_all(value, ",", "."),  # Replace commas with periods for proper decimal handling in R.
                  value = stringr::str_replace_all(value, "-", "0")) |> # According to "noegletal.dk", a dash "-" means 0.
    dplyr::mutate(value = as.numeric(value)) # Coerce to numeric, values of "M" or "U" (missing, according to Noegletal.dk), can't be coerced and is therefore made NA.

  # Pivot to tidy format
  data <- data |>
    tidyr::pivot_wider(id_cols = c(muni_code, year), names_from = variable, values_from = value)

  return(data)
}
