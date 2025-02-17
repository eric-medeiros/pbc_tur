vl_extra <- function(excel_extra) {
  library(readxl)
  library(dplyr)
  
  result <- 
    list(
      passeio_preco = read_xlsx(
        path = excel_extra, sheet = "passeio_preco", col_types = "text"),
      
      resposta_origem = read_xlsx(
        path = excel_extra, sheet = "resposta_origem", col_types = "text"),
      
      cidade_distancia = read_xlsx(
        path = excel_extra, sheet = "cidade_distancia", col_types = c("text", "text", "numeric", "text", "numeric")),
      
      preco_km_geral = read_xlsx(
        path = excel_extra, sheet = "preco_km_geral", col_types = c(
          "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"), skip = 2)
      %>% suppressWarnings(),
      
      tarifa_onibus_km = read_xlsx(
        path = excel_extra, sheet = "tarifa_onibus_km", col_types = c(
          "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip", "skip"), skip = 3),
      
      preco_pedagio = read_xlsx(path = excel_extra, sheet = "preco_pedagio", col_types = c(
        "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"), skip = 3),
      
      preco_alimentacao = read_xlsx(
        path = excel_extra, sheet = "preco_alimentacao", col_types = c("text", "text", "numeric","text"), skip = 2),
      
      cidade_posicao = read_xlsx(
        path = excel_extra, sheet = "cidade_posicao", col_types = c("text", "text", "numeric", "numeric")),
      
      hospedagem_diaria = read_xlsx(
        path = excel_extra, sheet = "hospedagem_diaria", col_types = c("text", "numeric"))
    )
  
  return (result)
  
}