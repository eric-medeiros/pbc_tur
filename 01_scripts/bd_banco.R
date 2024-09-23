bd_banco <- function( pasta_proj ) {
  source("01_scripts/bd_limpa_metadados.R")
  source("01_scripts/bd_limpa_entrevistas.R")
  
  # Caminhos para os arquivos RDS dos dados processados
  pasta_data <- file.path(pasta_proj, "00_data")
  caminho_RDS_bruto_metadados <- file.path(pasta_data, "SM_bruto", "metadados.rds")
  caminho_RDS_bruto_entrevistas <- file.path(pasta_data, "SM_bruto", "entrevistas.rds")
  
  dados_metadados <- readRDS(caminho_RDS_bruto_metadados)
  dados_entrevistas <- readRDS(caminho_RDS_bruto_entrevistas)
  
  metadados_limpo <- bd_limpa_metadados(dados_metadados)
  entrevistas_limpo <- bd_limpa_entrevistas(dados_entrevistas)
  
  bd <- list(
    perguntas = metadados_limpo,
    entrevistas = entrevistas_limpo$entrevistas,
    respostas = entrevistas_limpo$respostas
  )
  
  cat("-> Banco SurveyMonkey tratado a partir dos dados brutos \n")
  return(bd)
}
