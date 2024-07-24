bd_limpa_entrevistas <- function( pasta_output ) {
  library(dplyr)
  library(lubridate)
  library(tidyr)
  
  caminho_dados_brutos <- file.path(pasta_output, "SM_bruto")
  caminho_entrevistas_bruto <- list.files(caminho_dados_brutos, full.names = TRUE, pattern = "entrevistas")
  dados_entrevistas <- readRDS(caminho_entrevistas_bruto)

  # Criando base para criação de dados_perguntas e dados_respostas
  base <- 
  dados_entrevistas %>%
    mutate(
      `151230060` = as.character(`151230060`),
      `151230059` = as.character(`151230059`),
      date_created = ymd_hms(date_created, tz = "America/Sao_Paulo") %>% suppressMessages(),
      date_modified = ymd_hms(date_modified, tz = "America/Sao_Paulo") %>% suppressMessages(),
      date = as.Date(date_created),
      consentimento = case_when(
        `151230059` == "Eu li e concordo com o termo de consentimento e desejo continuar" ~ TRUE,
        `151230059` == "Eu discordo e/ou não irei continuar" ~ FALSE,
        TRUE ~ NA
      ),
      publico = case_when(
        `151230060` == "Estou visitando Canan\u00e9ia" ~ "TC",
        `151230060` == "Já visitei, mas não estou em Canan\u00e9ia" ~ "TF",
        `151230060` == "Moro em Canan\u00e9ia" ~ "M",
        `151230060` == "Desejo n\u00e3o responder (e encerrar o question\u00e1rio)" ~ "NR",
        is.na(`151230060`) ~ NA_character_,
        TRUE ~ NA_character_
      ),
      completo = case_when(
        response_status == "completed" ~ TRUE,
        response_status == "partial" ~ FALSE,
        TRUE ~ NA
      )
    )
  
  entrevistas <- 
    base %>%
    select(
      entrevista_num, date, collector_id, response_id, date_created, 
      date_modified, ip_address, completo, consentimento, publico
    )
  
  respostas <- 
    base %>%
    select(
      -collector_id, -response_status, -date_created,
      -date_modified, -ip_address, -`151230059`, -`151230060`) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(
      cols =  !response_id,
      names_to = "pergunta_resposta_id",
      values_to = "resposta") %>%
    separate_wider_delim(
      cols = pergunta_resposta_id,
      names = c("pergunta_id", "resposta_id"),
      delim = "_",
      too_few = "align_start"
    ) %>%
    group_by(pergunta_id, response_id) %>%
    nest() %>%
    group_by(pergunta_id) %>%
    nest()
  
  result <- list(
    entrevistas = entrevistas,
    respostas = respostas)
  
return(result)  
}
