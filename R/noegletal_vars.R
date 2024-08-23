noegletal_vars <- function() {

}

# Scrape variable definitions from noegletal.dk
source_code <- read_html("https://www.noegletal.dk/noegletal/ntInfo24A.html", encoding = "latin1")

var_headings <- source_code |>
  html_elements("div.ntal > a:first-child")

variable_ids <- var_headings |>
  html_attr("name") |>
  substr(3,5)

variable_names <- var_headings |>
  html_text(trim = TRUE)

variable_definitions <- source_code |>
  html_elements("div.ntal") |>
  html_elements(xpath = "following-sibling::div[contains(@class, 'txt')][1]") |>
  html_text(trim = TRUE)

variable_definitions[[155]] |>
  html_text(trim = TRUE)

df <- cbind(variable_ids, variable_names, variable_definitions) |>
  as_tibble()

# TODO: make a "noegletal_browse" function to browse variable definitions by the console.
