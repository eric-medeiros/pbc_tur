bd_fino <- function( pasta_output, update_fino = FALSE ) {
  
  # Caminhos para os arquivos RDS dos dados processados
  caminho_RDS_bruto_entrevistas <- file.path(pasta_output, "SM_bruto", "entrevistas.rds")
  caminho_RDS_bruto_perguntas <- file.path(pasta_output, "SM_bruto", "perguntas.rds")
  caminho_RDS_fino_banco <- file.path(pasta_output, "SM_fino", "banco_sm.rds")
  
  if ( update_fino ) {
    source("01_scripts/bd_limpa_metadados.R")
    source("01_scripts/bd_limpa_entrevistas.R")
    
    metadados_limpo <- bd_limpa_metadados(pasta_output)
    entrevistas_limpo <- bd_limpa_entrevistas(pasta_output)
    
    banco_sm <- list(
      perguntas = metadados_limpo,
      entrevistas = entrevistas_limpo$entrevistas,
      respostas = entrevistas_limpo$respostas
    )
    
    if(!dir.exists(dirname(caminho_RDS_fino_banco))) {dir.create(dirname(caminho_RDS_fino_banco))}
    saveRDS(banco_sm, caminho_RDS_fino_banco)
    cat("-> Banco SurveyMonkey tratado a partir dos dados brutos \n")
  } else {
    
    # Se não for necessário atualizar, ler os dados processados dos arquivos RDS
    banco_sm <- readRDS(caminho_RDS_fino_banco)
    cat("-> Dados de Banco SurveyMonkey tratados lidos\n")
  }
  return(banco_sm)
}