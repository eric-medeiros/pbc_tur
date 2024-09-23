bd_filtra_banco <- function(bd, pergunta_ID) {
  #jogar no chat
  
  tipo_publico <-
    bd$perguntas %>%
    filter(pergunta_id == pergunta_ID) %>%
    select(pergunta_publico) %>%
    first() %>%
    pull()
  
  ids <-  
    bd$respostas %>%
    filter(pergunta_id == '151230060') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    select()
    filter(publico == tipo_publico) %>%
    pull(response_id)
  
  dados <- 
    banco_sm$respostas %>%
    filter(pergunta_id == pergunta_ID) %>%
    unnest(cols = c(data)) %>%
    filter(response_id %in% ids) %>%
    unnest(cols = c(data))
  
  return(dados)
}