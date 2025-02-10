# R/anmData.R

#' anmData
#'
#' This function fetches data from the ANM open data portal.
#'
#' @param code A character vector with the series code.
#' @return A data frame containing the requested data.
#' @importFrom utils read.table
#' @importFrom utils data
#' @export

anmData <- function(code) {


  if (!code %in% rownames(anm_paths)) {
    stop("Invalid code! See available codes with `rownames(anm_paths)`.")
  }





  url_base <- "https://app.anm.gov.br/DadosAbertos"
  path <- anm_paths[code, 1]
  url <- paste0(url_base, path, ".csv")

  temp_file <- tempfile(fileext = ".csv")


  tryCatch({

    download.file(url = url, destfile = temp_file, mode = "wb")

      data <-
        read.table(
        file = url,
        header = TRUE,
        sep = ",",
        fill = TRUE,
        stringsAsFactors = FALSE,
        fileEncoding = "Latin1",
        quote = "\"",
        check.names = FALSE,
        dec = ","
      )
      return(data)
    },
    error = function(e) {
      stop("Error downloading data. Check your connection or the code provided.")
    }
  )
}
