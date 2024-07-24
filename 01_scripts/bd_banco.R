bd_banco <- function(pasta_outputs, update_bruto = FALSE, update_fino = FALSE) {
  source("01_scripts/bd_fino.R")
  pasta_output <- file.path("03_results")
  
  if(update_bruto) {
    source("01_scripts/bd_bruto.R")
    bd_bruto(pasta_output, update_bruto)
    banco <- bd_fino(pasta_output, update_fino)
    
  } else {
    banco <- bd_fino(pasta_output, update_fino)
  }
  return(banco)
}