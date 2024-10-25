# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

available_series <-
  function (language = c("en", "br"))
  {
    language <- match.arg(language)
    url <- "http://www.ipeadata.gov.br/api/odata4/Metadados/"
    series <- NULL
    if (curl::has_internet()) {
      Sys.sleep(0.01)
      tryCatch({
        series <- jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
          dplyr::as_tibble() %>% dplyr::select(
            .data$SERCODIGO,
            .data$SERNOME,
            .data$BASNOME,
            .data$FNTSIGLA,
            .data$PERNOME,
            .data$SERATUALIZACAO,
            .data$SERSTATUS
          ) %>%
          dplyr::arrange(.data$BASNOME,
                         .data$FNTSIGLA,
                         .data$PERNOME,
                         .data$SERCODIGO) %>% dplyr::mutate(FNTSIGLA = as.factor(.data$FNTSIGLA)) %>%
          dplyr::mutate(SERATUALIZACAO = lubridate::as_date(.data$SERATUALIZACAO)) %>%
          dplyr::mutate(SERSTATUS = as.character(.data$SERSTATUS)) %>%
          dplyr::mutate(SERSTATUS = dplyr::if_else(is.na(.data$SERSTATUS),
                                                   "", .data$SERSTATUS))
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      if (!is.null(series)) {
        if (language == "en") {
          series %<>% dplyr::mutate(SERSTATUS = factor(
            .data$SERSTATUS,
            levels = c("A", "I", ""),
            labels = c("Active",
                       "Inactive", "")
          )) %>% dplyr::mutate(PERNOME = iconv(.data$PERNOME,
                                               "UTF-8", "ASCII//TRANSLIT")) %>% dplyr::mutate(PERNOME = factor(
                                                 .data$PERNOME,
                                                 levels = c(
                                                   "Anual",
                                                   "Decenal",
                                                   "Diaria",
                                                   "Irregular",
                                                   "Mensal",
                                                   "Quadrienal",
                                                   "Quinquenal",
                                                   "Semestral",
                                                   "Trimestral",
                                                   "Nao se aplica"
                                                 ),
                                                 labels = c(
                                                   "Yearly",
                                                   "Decennial",
                                                   "Daily",
                                                   "Irregular",
                                                   "Monthly",
                                                   "Quadrennial",
                                                   "Quinquennial",
                                                   "Semiannual",
                                                   "Quarterly",
                                                   "Not applicable"
                                                 )
                                               )) %>% dplyr::mutate(BASNOME = iconv(.data$BASNOME,
                                                                                    "UTF-8", "ASCII//TRANSLIT")) %>% dplyr::mutate(BASNOME = factor(
                                                                                      .data$BASNOME,
                                                                                      levels = c("Macroeconomico", "Regional", "Social"),
                                                                                      labels = c("Macroeconomic", "Regional", "Social")
                                                                                    )) %>%
            purrr::set_names(c(
              "code",
              "name",
              "theme",
              "source",
              "freq",
              "lastupdate",
              "status"
            )) %>%
            sjlabelled::set_label(
              c(
                "Ipeadata Code",
                "Serie Name (PT-BR)",
                "Theme",
                "Source",
                "Frequency",
                "Last Update",
                "Status"
              )
            )
        }
        else {
          series %<>% dplyr::mutate(SERSTATUS = factor(
            .data$SERSTATUS,
            levels = c("A", "I", ""),
            labels = c("Ativa",
                       "Inativa", "")
          )) %>% dplyr::mutate(BASNOME = factor(.data$BASNOME)) %>%
            dplyr::mutate(PERNOME = factor(.data$PERNOME)) %>%
            purrr::set_names(c(
              "code",
              "name",
              "theme",
              "source",
              "freq",
              "lastupdate",
              "status"
            )) %>%
            sjlabelled::set_label(
              c(
                "Codigo Ipeadata",
                "Nome da Serie",
                "Nome da Base",
                "Fonte",
                "Frequencia",
                "Ultima Atualizacao",
                "Status"
              )
            )
        }
      }
    }
    else {
      message("The internet connection is unavailable.")
    }
    series
  }



anmdata <-
  function (code,
            language = c("en", "br"),
            quiet = FALSE)
  {
    language <- match.arg(language)
    values <- dplyr::as_tibble(data.frame(NULL))
    if (!quiet & (length(code) >= 2)) {
      cat("Requesting Ipeadata API <http://www.ipeadata.gov.br/>")
      cat("\n")
      pb <- txtProgressBar(min = 0,
                           max = length(code),
                           style = 3)
    }
    update.step <- max(2, floor(length(code) / 100))
    if (curl::has_internet()) {
      Sys.sleep(0.01)
      tryCatch({
        for (i in 1:length(code)) {
          code0 <- gsub(" ", "_", toupper(code[i]))
          url <-
            paste0(
              "http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='",
              code0,
              "')"
            )
          Sys.sleep(0.01)
          values.aux <- dplyr::as_tibble(jsonlite::fromJSON(url,
                                                            flatten = TRUE)[[2]])
          if (length(values.aux) > 0) {
            values.aux %<>% dplyr::mutate(TERCODIGO = as.integer(.data$TERCODIGO)) %>%
              dplyr::mutate(NIVNOME = as.factor(.data$NIVNOME)) %>%
              dplyr::mutate(VALDATA = lubridate::as_date(.data$VALDATA)) %>%
              dplyr::arrange(.data$TERCODIGO, .data$VALDATA)
            values <- rbind(values, values.aux)
          }
          else {
            warning(paste0("code '", code[i], "' not found"))
          }
          if (!quiet & (i %% update.step == 0 | i == length(code)) &
              (length(code) >= 2)) {
            setTxtProgressBar(pb, i)
          }
        }
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
    }
    else {
      message("The internet connection is unavailable.")
    }
    if (!quiet & (length(code) >= 2)) {
      close(pb)
    }
    if (nrow(values) != 0) {
      values %<>% dplyr::filter(!is.na(.data$VALVALOR)) %>%
        dplyr::distinct()
      if (language == "en") {
        values %<>% dplyr::mutate(NIVNOME = iconv(.data$NIVNOME,
                                                  "UTF-8", "ASCII//TRANSLIT")) %>% dplyr::mutate(NIVNOME = factor(
                                                    .data$NIVNOME,
                                                    levels = c(
                                                      "Brasil",
                                                      "Regioes",
                                                      "Estados",
                                                      "Mesorregioes",
                                                      "Microrregioes",
                                                      "Estado/RM",
                                                      "Area metropolitana",
                                                      "Municipios",
                                                      "AMC 91-00",
                                                      "AMC 70-00",
                                                      "AMC 60-00",
                                                      "AMC 40-00",
                                                      "AMC 20-00",
                                                      "AMC 1872-00",
                                                      ""
                                                    ),
                                                    labels = c(
                                                      "Brazil",
                                                      "Regions",
                                                      "States",
                                                      "Mesoregions",
                                                      "Microregions",
                                                      "State/Metropolitan region",
                                                      "Metropolitan area",
                                                      "Municipality",
                                                      "MCA 91-00",
                                                      "MCA 70-00",
                                                      "MCA 60-00",
                                                      "MCA 40-00",
                                                      "MCA 20-00",
                                                      "MCA 1872-00",
                                                      ""
                                                    ),
                                                    ordered = TRUE
                                                  )) %>% purrr::set_names(c("code",
                                                                            "date", "value", "uname", "tcode")) %>% sjlabelled::set_label(
                                                                              c(
                                                                                "Ipeadata Code",
                                                                                "Date",
                                                                                "Value",
                                                                                "Territorial Unit Name",
                                                                                "Country or Territorial Code"
                                                                              )
                                                                            )
      }
      else {
        values %<>% dplyr::mutate(NIVNOME = factor(
          .data$NIVNOME,
          levels = levels(factor(.data$NIVNOME))[c(
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == ""),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "Brasil"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "Regioes"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "Estados"),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "Mesorregioes"
            ),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "Microrregioes"
            ),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "Estado/RM"
            ),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "Area metropolitana"
            ),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "Municipios"
            ),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "AMC 91-00"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "AMC 70-00"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "AMC 60-00"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "AMC 40-00"),
            which(iconv(
              levels(factor(.data$NIVNOME)),
              "UTF-8", "ASCII//TRANSLIT"
            ) == "AMC 20-00"),
            which(
              iconv(levels(factor(
                .data$NIVNOME
              )),
              "UTF-8", "ASCII//TRANSLIT") == "AMC 1872-00"
            )
          )],
          ordered = TRUE
        )) %>% purrr::set_names(c("code",
                                  "date", "value", "uname", "tcode")) %>% sjlabelled::set_label(
                                    c(
                                      "Codigo Ipeadata",
                                      "Data",
                                      "Valor",
                                      "Nome da Unidade Territorial",
                                      "Codigo de Pais ou Territorial"
                                    )
                                  )
      }
    }
    values
  }





Producao_Bruta <-
  read.table(
    file = "https://app.anm.gov.br/dadosabertos/AMB/Producao_Bruta.csv" # './_____b/Producao_Bruta.csv'
    ,
    header = TRUE
    ,
    sep = ","
    ,
    quote = "\""
    ,
    dec = ","
    ,
    fileEncoding = "Latin1"
  )


Producao_Beneficiada <-
  read.table(
    file = "https://app.anm.gov.br/dadosabertos/AMB/Producao_Beneficiada.csv" # './_____b/Producao_Bruta.csv'
    ,
    header = TRUE
    ,
    sep = ","
    ,
    quote = "\""
    ,
    dec = ","
    ,
    fileEncoding = "Latin1"
  )
