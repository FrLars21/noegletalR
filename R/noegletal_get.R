### CONSTANTS
allowed_muni_codes <- c(101, 147, 151, 153, 155,
                        157, 159, 161, 163, 165,
                        167, 169, 173, 175, 183,
                        185, 187, 190, 201, 210,
                        217, 219, 223, 230, 240,
                        250, 253, 259, 260, 265,
                        269, 270, 306, 316, 320,
                        326, 329, 330, 336, 340,
                        350, 360, 370, 376, 390,
                        400, 410, 420, 430, 440,
                        450, 461, 479, 480, 482,
                        492, 510, 530, 540, 550,
                        561, 563, 573, 575, 580,
                        607, 615, 621, 630, 657,
                        661, 665, 671, 706, 707,
                        710, 727, 730, 740, 741,
                        746, 751, 756, 760, 766,
                        773, 779, 787, 791, 810,
                        813, 820, 825, 840, 846,
                        849, 851, 860)

allowed_years <- seq.int(2007,2024)

###

noegletal_scrape <- function(muni_codes, years, variable_ids) {
  muni_codes_string <- paste(muni_codes, collapse = "%2C")
  years_string <- paste(years, collapse = "%2C")
  variable_ids_string <- paste(variable_ids, collapse = "%2C")

  payload <- paste0("qUdv=1",
                    "&qUdvBasis=0",
                    "&qSort=1",
                    "&qSortBasis=0",
                    "&qSaveAs=0",
                    "&qCommand=Rap",
                    "&qReform=1",
                    "&qListAar=", years_string,
                    "&qListKom=", muni_codes_string,
                    "&qListNog=", variable_ids_string,
                    "&qPreKom=Alle%2Bkommuner",
                    "&qPreKomNr=921",
                    "&qPreNog=x",
                    "&qPreNogNr=0",
                    "&qPris=1",
                    "&qPrisBasis=0",
                    "&qKomInddel=0",
                    "&qHighlight=0",
                    "&qHighl_kom=0",
                    "&qVisKunGns=0",
                    "&qVers=24A",
                    "&qHtmlVers=43",
                    "&qLevel=1",
                    "&qResVs=42",
                    "&qUserResol=X",
                    "&qUserAppl=X",
                    "&qUserAgent=X")

  response <- httr::VERB("POST",
                   url = "https://www.noegletal.dk/noegletal/nctrlman",
                   body = payload, content_type("application/x-www-form-urlencoded"),
                   encode = "form")

  source_code <- httr::content(response, "parsed", encoding = "latin1")
  return(source_code)
}

noegletal_parse <- function(source_code, n_munis, years, variable_ids) {
  table_rows <- source_code |>
    rvest::html_elements(xpath = "//tr[td[@class = 'nwDatL' or contains(concat(' ', normalize-space(@class), ' '), ' nwDatL ')]]")

  data_matrix <- NULL
  for (tr in table_rows) {
    td_text <- tr |>
      rvest::html_elements("td") |>
      rvest::html_text(trim = TRUE)

    data_matrix <- rbind(data_matrix, td_text)
  }

  df <- data_matrix |>
    tibble::as_tibble() |> # convert matrix to tibble
    dplyr::select(-1) # delete the first col (it is empty)

  names(df) <- c("muni_code", years)

  # parse variable names
  var_names <- source_code |>
    rvest::html_elements(".nwGrp") |>
    rvest::html_text(trim = TRUE)

  df <- df |>
    dplyr::mutate(variable = rep(var_names, each = n_munis)[1:nrow(df)], .after = muni_code) |>
    dplyr::mutate(muni_name = stringr::str_split_fixed(muni_code, " ", 2)[,2], .after = muni_code) |>
    dplyr::mutate(muni_code = stringr::str_split_fixed(muni_code, " ", 2)[,1])

  return(df)
}

noegletal_wrangle <- function(df) {
  df <- df |>
    tidyr::pivot_longer(cols = 4:ncol(df), names_to = "year") |> # Pivot to long format
    dplyr::mutate(value = str_replace_all(value, "-", "0")) |> # According to "noegletal.dk", a dash "-" means 0.
    dplyr::mutate(value = as.numeric(value)) |> # Coerce to numeric, values of "M" (missing, according to Noegletal.dk), can't be coerced and is therefore made NA.
    tidyr::pivot_wider(id_cols = c(muni_code, muni_name, year), names_from = variable, values_from = value) # Pivot to tidy format

  return(df)
}

#' Automatically get requested data from noegletal.dk
#'
#' `noegletal_get` make it possible to get data from noegletal.dk directly from the comfort of R.
#'
#' @param muni_codes A vector of municipalities to include in the dataset, as identified by their official municipality codes.
#' @param years A vector of years to include in the dataset (2007-current).
#' @param variables A vector of variables (nøgletal) to include in the dataset, as identified by the internal noegletal.dk variable id's.
#'
#' @returns A tidy `tibble` with one row for each municipality-year, and one column for each included variable (nøgletal).
#'
#' @export
noegletal_get <- function(muni_codes, years, variable_ids) {
  print("Getting requested data from noegletal.dk ..")

  # check year
  # check muni_code

  # Ensure that a max of 25000 rows is selected
  n_selected_rows <- length(muni_codes) + length(years) + length(variable_ids)
  if(!n_selected_rows > 25000) {
    years <- sort(years) # sort years in chronological order for proper alignment.

    source_code <- noegletal_scrape(muni_codes, years, variable_ids)
    parsed_data <- noegletal_parse(source_code, n_munis = length(muni_codes), years)
    df <- noegletal_wrangle(parsed_data)

    return(df)
  } else {
    print("too many rows selected (a maximum of 25000 is allowed)")
  }

}

# TODO: validate input years and muni_codes.
# TODO: standard params (all munis, all years).

# TODO: validate input variable ids.
# TODO: cached requests.

noegletal_get(muni_codes = c(101, 155), years=c(2007,2008), variable_ids = c(1,155))

t <- noegletal_scrape(muni_codes = c(101, 155), years=c(2007,2008), variable_ids = c(1,155))
df <- noegletal_parse(source_code = t, n_munis = 2, years=c(2007,2008), variable_ids = c(1,155))
df2 <- noegletal_wrangle(df)
