vl_extra <- function(excel_extra) {
  library(readxl)
  library(dplyr)
  
  passeio_preco <- read_xlsx(path = excel_extra, sheet = "passeio_preco", col_types = "text")
  resposta_origem <- read_xlsx(path = excel_extra, sheet = "resposta_origem", col_types = "text")
  cidade_distancia <- read_xlsx(path = excel_extra, sheet = "cidade_distancia", col_types = "text")
  cidade_preco <- read_xlsx(path = excel_extra, sheet = "cidade_preco", col_types = c(rep("text", 10), rep("skip",3)))
  cidade_posicao <- read_xlsx(path = excel_extra, sheet = "cidade_posicao", col_types = "text")
  hospedagem_diaria <- read_xlsx(path = excel_extra, sheet = "hospedagem_diaria", col_types = "text")
  
  result <- 
    list(
      passeio_preco = passeio_preco,
      cidade_distancia = cidade_distancia,
      cidade_posicao = cidade_posicao,
      resposta_origem = resposta_origem,
      ciadade_preco = cidade_preco,
      hospedagem_diaria = hospedagem_diaria
    )
  
  return (result)
  
}