bd_baixa_bruto <- function( id_survey = 514390303 ) {
  library(svmkR)
  library(dplyr)
  
  metadados <- fetch_survey_obj(id_survey)
  
  entrevistas <- 
    fetch_survey_obj(id_survey) %>%
    parse_survey()
  
  entrevistas <-
    entrevistas %>%
    mutate(entrevista_num = row_number()) %>% 
    select(
      -collection_mode,
      -survey_id,
      -first_name,
      -last_name,
      -email_address)
  
  dados_brutos <- list(
    metadados = metadados,
    entrevistas = entrevistas)
  return(dados_brutos)
}