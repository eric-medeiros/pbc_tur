vl_banco <- function(bd){
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  # MOTIVO ----
  motivo <- 
    bd$respostas %>%
    filter(pergunta_id == '162823740') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    group_by(response_id) %>%
    select(-pergunta_id) %>%
    nest() %>%
    mutate(motivo_boto = map_lgl(data, ~ any(.x$resposta_id == '1192079029' & .x$resposta == "TRUE", na.rm = TRUE))) %>%
    rename(motivo_data = data)
  
  # PÚBLICO ----
  publico <- 
    bd$respostas %>%
    filter(pergunta_id == '151230060') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    group_by(response_id) %>%
    mutate(publico = case_match(
      resposta,
      "Desejo n\u00e3o responder (e encerrar o question\u00e1rio)" ~ NA,
      "Estou visitando Canan\u00e9ia" ~ "T",
      "Moro em Canan\u00e9ia" ~ "M"
    ))
  
  # MEIO DE TRANSPORTE ----
  meio_outros <- 
    bd$respostas %>%
    filter(pergunta_id == '151230079') %>%
    unnest(data) %>%
    unnest(data) %>%
    filter(resposta_id == '1113887792') %>%
    mutate(resposta = case_match(resposta, "Blabacar" ~ "Carona", .default = resposta)) %>%
    ungroup() %>%
    select(-pergunta_id, -resposta_id)
  
  meio_alt <- 
    bd$respostas %>%
    filter(pergunta_id == '151230079') %>%
    unnest(data) %>%
    unnest(data) %>%
    filter(is.na(resposta_id)) %>%
    ungroup() %>%
    select(-pergunta_id, -resposta_id)
  
  meio <- 
    meio_alt %>%
    inner_join(meio_outros, by = "response_id") %>%
    unite("meio", 2:3, remove = TRUE, na.rm = TRUE) %>%
    mutate(meio = case_when(meio == "" ~ "Não respondeu", .default = meio))
  
  # TEMPO DE PERMANÊNCIA ----
  perm <- 
    bd$respostas %>%
    filter(pergunta_id == '151230078') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    select(response_id, perm = resposta)
  
  # RENDA ----  
  renda_tur <-
    bd$respostas %>%
    filter(pergunta_id == '151230083') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    select(response_id, resposta)
  
  renda_mor <-
    bd$respostas %>%
    filter(pergunta_id == '151426421') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    select(response_id, resposta)
  
  renda <- 
    inner_join(renda_tur, renda_mor, by = "response_id") %>%
    unite(col = resposta, c("resposta.x","resposta.y"), na.rm = TRUE) %>%
    group_by(response_id) %>%
    transmute(renda = case_when(
      resposta == "" ~ NA_character_,
      resposta == "Desejo n\u00e3o informar" ~ NA_character_,
      resposta == "0 a 1.320 (At\u00e9 1 sal\u00e1rio M\u00ednimo)" ~ "660",
      resposta == "1.321 a 2.640 (de 1 a 2 sal\u00e1rios m\u00ednimos)" ~ "1980.5",
      resposta == "2.641 a 6.600 (de 2 a 5 sal\u00e1rios m\u00ednimos)" ~ "4620.5",
      resposta == "6.601 a 13.200 (de 5 a 10 sal\u00e1rios m\u00ednimos)" ~ "9900.5",
      resposta == "13.201+ (mais de 10 sal\u00e1rios m\u00ednimos)" ~ "13200",
      .default = resposta
    ))
  
  # HOSPEDAGEM ----
  hosp_outros <- 
    bd$respostas %>%
    filter(pergunta_id == '151230076') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    filter(resposta_id == '1113887765') %>%
    select(-pergunta_id, -resposta_id) %>%
    mutate(resposta = case_match(
      resposta,
      c("Casa",
        "Casa ",
        "Casa própria ",
        "Mora ") ~ "Casa Própria",
      c("Alojamento ",
        "Alojamento") ~ "Alojamento",
      c("Nenhum ",
        "Nenhum",
        "Nenhum.",
        "Nenhuma",
        "Nenhuma ",
        "Nenhu,",
        "Sem hospedagem ",
        "Um dia na cidade ",
        "Um dia",
        "Um dia ",
        "1",
        "1 dia",
        "Um dia em cananeia",
        "Não irá ficar",
        "Não ",
        "Estou em ilha comprida na casa de familiares",
        "Está em miracatu",
        "Bate e volta",
        "Bate e volta ",
        "Bate volta") ~ "Não fiquei hospedado" 
    ))
  
  hosp_alt <- 
    bd$respostas %>%
    filter(pergunta_id == '151230076') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    filter(is.na(resposta_id)) %>%
    select(-pergunta_id, -resposta_id)
  
  hosp <- 
    hosp_alt %>% 
    inner_join(hosp_outros, by = "response_id") %>%
    unite("hospedagem", 2:3, remove = TRUE, na.rm = TRUE) %>%
    mutate(hospedagem = case_when(hospedagem == "" ~ NA_character_, .default = hospedagem))
  
  # ACOMPANHANTES ----
  cia_outros <- 
    bd$respostas %>%
    filter(pergunta_id == '151230111') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    filter(resposta_id == '1113888181') %>%
    select(-pergunta_id, -resposta_id)
  
  cia_alt <- 
    bd$respostas %>%
    filter(pergunta_id == '151230111') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    filter(is.na(resposta_id)) %>%
    select(-pergunta_id, -resposta_id)
  
  cia <-
    inner_join(cia_alt, cia_outros, by = "response_id") %>%
    unite("cia", 2:3, remove = TRUE, na.rm = TRUE) %>%
    mutate(cia = case_when(cia == "" ~ NA_character_, .default = cia))
  
  # NÚMERO DE VIAJANTES ----
  pessoas_n <-  
    bd$respostas %>%
    filter(pergunta_id == '151230075') %>%
    unnest(data) %>%
    unnest(data) %>%
    ungroup() %>%
    mutate(n_pessoas = case_match(
      resposta,
      "Estou sozinho(a)" ~ "1",
      "1 outra pessoa" ~ "2",
      "2 outras pessoas" ~ "3",
      "3 outras pessoas" ~ "4",
      "mais de 4 outras pessoas" ~ "5+",
      "Desejo não responder" ~ NA_character_
    )) %>%
    select(-pergunta_id, -resposta_id)
  
  # PASSEIO ----
  opcoes_tur <- 
    bd$perguntas %>%
    filter(pergunta_id == '151230104') %>%
    select(resposta_id, resposta_nome)
  
  passeio_tur <- 
    bd$respostas %>%
    filter(pergunta_id == '151230104') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    group_by(response_id) %>%
    left_join(opcoes_tur, by = "resposta_id") %>%
    filter(!is.na(resposta) & !is.na(resposta_nome)) %>%
    select(-pergunta_id, -resposta)
  
  opcoes_mor <- 
    bd$perguntas %>%
    filter(pergunta_id == '151499315') %>%
    select(resposta_id, resposta_nome)
  
  passeio_mor <- 
    bd$respostas %>%
    filter(pergunta_id == '151499315') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    group_by(response_id) %>%
    left_join(opcoes_mor, by = "resposta_id") %>%
    filter(!is.na(resposta) & !is.na(resposta_nome)) %>%
    select(-pergunta_id, -resposta)
  
  passeio <- 
    bind_rows(passeio_mor, passeio_tur) %>%
    nest() %>%
    rename(passeio_data = data)
  
  # PREVISÃO DE GASTOS
  gasto_previsto_i <- 
    bd$respostas %>%
    filter(pergunta_id == '151230077') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    mutate(gasto_i = case_match(
      resposta,
      "0 — 200" ~ "100",
      "200 — 500" ~ "350",
      "500 — 1.000" ~ "750",
      "1.000 — 2.000" ~ "1500",
      "2.000 — 5.000" ~ "3500",
      "5.000 — 10.000" ~ "7500",
      "10.000+" ~ "10000",
      "Desejo não responder" ~ NA_character_
    )) %>%
    select(response_id, gasto_i)

  gasto_previsto_f <- 
  bd$respostas %>%
    filter(pergunta_id == '151230110') %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    mutate(gasto_f = case_match(
      resposta,
      "0 — 200" ~ "100",
      "200 — 500" ~ "350",
      "500 — 1.000" ~ "750",
      "1.000 — 2.000" ~ "1500",
      "2.000 — 5.000" ~ "3500",
      "5.000 — 10.000" ~ "7500",
      "10.000+" ~ "10000",
      "Desejo não responder" ~ NA_character_
    )) %>%
    select(response_id, gasto_f)
  
  gasto <- 
    inner_join(gasto_previsto_f, gasto_previsto_i, by = "response_id") %>%
    unite(gasto, 2:3, na.rm = TRUE) %>%
    mutate(gasto = case_when(gasto == "" ~ NA_character_, .default = gasto))
  
  # JUNTANDO TUDO ----
  result <-
    list(
      motivo,
      publico,
      meio,
      perm,
      renda,
      hosp,
      cia,
      gasto,
      pessoas_n,
      passeio
    ) %>%
    reduce(full_join, by = "response_id") %>%
    select(response_id, motivo_boto, publico, meio, perm, renda, hospedagem, cia, gasto, n_pessoas, passeio_data) %>%
    ungroup() %>%
    mutate(publico_motivo = case_when(
      motivo_boto == TRUE & publico == "T" ~ "Turistas com motivo",
      motivo_boto == FALSE & publico == "T" ~ "Turistas sem motivo",
      publico == "M" ~ "Moradores",
      TRUE ~ "NA")) %>%
    group_by(response_id)
    
    return(result)
}




