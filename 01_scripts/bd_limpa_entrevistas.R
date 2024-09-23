bd_limpa_entrevistas <- function( dados_entrevistas ) {
  library(dplyr)
  library(lubridate)
  library(tidyr)
  
  # Criando base para criação de dados_perguntas e dados_respostas
  base <- 
    dados_entrevistas %>%
    mutate(
      date_created = ymd_hms(date_created, tz = "America/Sao_Paulo") %>% suppressMessages(),
      date_modified = ymd_hms(date_modified, tz = "America/Sao_Paulo") %>% suppressMessages(),
      date = as.Date(date_created),
      completo = case_when(
        response_status == "completed" ~ TRUE,
        response_status == "partial" ~ FALSE,
        TRUE ~ NA
      )
    ) %>% 
    filter(collector_id != "452948515" & collector_id != "453886430")
  
  entrevistas <- 
    base %>%
    select(
      entrevista_num, date, collector_id, response_id,
      date_created, date_modified, ip_address, completo
    )
  
  respostas <- 
    base %>%
    select(
      -collector_id, -response_status, -date_created,
      -date_modified, -ip_address) %>%
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