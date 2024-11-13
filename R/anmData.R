# R/anmData.R

#' Fetch ANM Data
#'
#' This function fetches data from the ANM open data portal.
#'
#' @param code A character vector with the series code.
#' @return A data frame containing the requested data.
#' @export

anmData <- function(code) {

  url <- "https://app.anm.gov.br/DadosAbertos/"
  read.table(
    file = paste0(url, code),
    header = TRUE,
    sep = ",",
    fill = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "Latin1",
    quote = "\""
  )

}


# fetch_anm_data <- function(endpoint) {
#   base_url <- "https://dados.gov.br/dataset/anm-dados-abertos"
#   url <- paste0(base_url, endpoint)
#
#   response <- httr::GET(url)
#
#   if (response$status_code == 200) {
#     data <- jsonlite::fromJSON(content(response, "text"))
#     return(dplyr::as_tibble(data))
#   } else {
#     stop("Failed to fetch data from ANM. Status code: ", response$status_code)
#   }
# }










