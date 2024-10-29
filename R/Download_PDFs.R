library(rvest)
library(httr)


lista <- 
  list("https://app.anm.gov.br/DadosAbertos/AMB/",
       "https://app.anm.gov.br/DadosAbertos/ARRECADACAO/",
       "https://app.anm.gov.br/DadosAbertos/BARRAGEM/",
       "https://app.anm.gov.br/DadosAbertos/PROCURADORIA/",
       "https://app.anm.gov.br/DadosAbertos/PROJUR/",
       "https://app.anm.gov.br/DadosAbertos/SCM/",
       "https://app.anm.gov.br/DadosAbertos/SICOP/",
       "https://app.anm.gov.br/DadosAbertos/SIGMINE/",
       "https://app.anm.gov.br/DadosAbertos/SOPLE/")



for (i in 1:length(lista)) {
  

# URL principal
url_base <- lista[[i]]
urls_visitadas <- character()

# Função para extrair e retornar todos os links de uma página específica
extrair_links <- function(url) {
  tryCatch({
    pagina <- read_html(url)
    links <- pagina %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      na.omit()
    # Completar URLs relativas
    links <- ifelse(grepl("^http", links), links, paste0(url_base, links))
    return(links)
  }, error = function(e) {
    return(character())
  })
}

# Função para baixar PDFs
baixar_pdf <- function(link) {
  nome_arquivo <- basename(link)
  caminho_destino <- file.path("pdfs", nome_arquivo)
  if (!file.exists(caminho_destino)) {
    GET(link, write_disk(caminho_destino, overwrite = TRUE))
    cat("Baixado:", nome_arquivo, "\n")
  } else {
    cat("Já existe:", nome_arquivo, "\n")
  }
}

# Função principal para navegar por páginas e baixar PDFs
buscar_pdfs <- function(url) {
  if (url %in% urls_visitadas) return() # Evitar visitar repetidamente
  
  cat("Visitando:", url, "\n")
  urls_visitadas <<- c(urls_visitadas, url) # Marcar como visitada
  
  links <- extrair_links(url)
  
  # Filtrar e baixar PDFs
  pdf_links <- links[grepl("\\.pdf$", links)]
  walk(pdf_links, baixar_pdf)
  
  # Links a visitar (filtrar subdomínios ou URLs no mesmo domínio)
  sub_paginas <- links[grepl(url_base, links) & !grepl("\\.pdf$", links)]
  walk(sub_paginas, buscar_pdfs) # Recursivamente visitar as páginas
}

# Criar pasta para salvar PDFs, se não existir
if (!dir.exists("pdfs")) {
  dir.create("pdfs")
}

# Iniciar a busca e download dos PDFs
buscar_pdfs(url_base)

}