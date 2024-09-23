bd_fino <- function( pasta_results ) {
  source("01_scripts/bd_limpa_metadados.R")
  source("01_scripts/bd_limpa_entrevistas.R")
  
  # Caminhos para os arquivos RDS dos dados processados
  caminho_RDS_bruto_entrevistas <- file.path(pasta_results, "SM_bruto", "entrevistas.rds")
  caminho_RDS_bruto_perguntas <- file.path(pasta_results, "SM_bruto", "perguntas.rds")
  caminho_RDS_fino_banco <- file.path(pasta_results, "SM_fino", "banco_sm.rds")
  
  metadados_limpo <- bd_limpa_metadados(pasta_results)
  entrevistas_limpo <- bd_limpa_entrevistas(pasta_results)
  
  banco_sm <- list(
    perguntas = metadados_limpo,
    entrevistas = entrevistas_limpo$entrevistas,
    respostas = entrevistas_limpo$respostas
  )
  
  cat("-> Banco SurveyMonkey tratado a partir dos dados brutos \n")
  return(banco_sm)
  
}