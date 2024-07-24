bd_bruto <- function( pasta_output, update_bruto = FALSE){
    caminho_RDS_metadados <- file.path(pasta_output, "SM_bruto", "metadados.rds")
    caminho_RDS_entrevistas  <- file.path(pasta_output, "SM_bruto", "entrevistas.rds")
    
    if (update_bruto == TRUE) {
      stop("Na na na... Tem que pagar o SM!!!")
      source("01_scripts/.bd_baixa_bruto.R")
      if(!dir.exists(pasta_output)) {dir.create(pasta_output)}
      if(!dir.exists(dirname(caminho_RDS_entrevistas))) {dir.create(dirname(caminho_RDS_entrevistas))}
      dados_brutos <- bd_baixa_bruto()
      saveRDS(dados_bruto$metadados, file = caminho_RDS_metadados)
      saveRDS(dados_bruto$entrevistas, file = caminho_RDS_entrevistas)
      cat(
        "-> dados e metadados brutos do SM salvos em: \n",
        caminho_RDS_metadados, " e ", caminho_RDS_entrevistas
      )
    } else if(update_bruto == FALSE) {
      metadados <- readRDS(caminho_RDS_metadados)
      entrevistas <- readRDS(caminho_RDS_entrevistas)
      dados_brutos <- list(
        metadados = metadados,
        entrevistas = entrevistas)
      cat(
        "-> dados e metadados brutos do SM lidos de: \n",
        caminho_RDS_metadados, " e ", caminho_RDS_entrevistas
      )
    }
    return(dados_brutos)
}