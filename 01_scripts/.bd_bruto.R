bd_bruto <- function( pasta_results ){
    caminho_RDS_metadados <- file.path(pasta_results, "SM_bruto", "metadados.rds")
    caminho_RDS_entrevistas  <- file.path(pasta_results, "SM_bruto", "entrevistas.rds")
    
      metadados <- readRDS(caminho_RDS_metadados)
      entrevistas <- readRDS(caminho_RDS_entrevistas)
      dados_brutos <- list(
        metadados = metadados,
        entrevistas = entrevistas)
      cat(
        "-> dados e metadados brutos do SM lidos de: \n",
        caminho_RDS_metadados, " e ", caminho_RDS_entrevistas
      )
      return(dados_brutos)
}