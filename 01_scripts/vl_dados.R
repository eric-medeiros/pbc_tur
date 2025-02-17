vl_dados <- function(pasta_proj) {
  # Carregar pacotes
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  source("01_scripts/bd_banco.R")
  source("01_scripts/vl_banco.R")
  source("01_scripts/vl_extra.R")
  
  # Definindo caminhos
  excel_extra <- file.path(pasta_proj, "00_data", "valoracao_extra.xlsx")
  
  # Pegando banco de dados e excel
  bd <- bd_banco(pasta_proj)
  dados_banco <- vl_banco(bd)
  dados_excel <- vl_extra(excel_extra)
  
  # Função auxiliar para arredondar valores numéricos ou convertíveis em numéricos
  round_if_numeric <- function(x, digits = 2) {
    if (is.character(x) && !is.na(suppressWarnings(as.numeric(x)))) {
      x <- as.numeric(x)
    }
    if (is.numeric(x)) {
      round(x, digits)
    } else {
      NA_real_  # Se não for numérico ou convertível, retornar NA
    }
  }
  
  # Cálculo de custo de transporte
  custo_transporte <- 
    dados_banco %>%
    select(response_id, meio) %>%
    left_join(dados_excel$resposta_origem %>% select(response_id, municipio, uf), by = "response_id") %>%
    left_join(dados_excel$cidade_distancia %>% select(municipio, uf, km_rod), by = c("municipio", "uf")) %>%
    left_join(dados_excel$preco_km_geral %>% select(meio, custo_km_individual), by = "meio") %>%
    mutate(custo_transporte = custo_km_individual * km_rod)
  
  # Cálculo de custo de hospedagem
  custo_hospedagem <- 
    dados_banco %>%
    select(response_id, hospedagem, perm) %>%
    left_join(dados_excel$hospedagem_diaria, by = "hospedagem") %>%
    mutate(custo_hospedagem = preco_diaria * as.numeric(perm))
  
  # Cálculo de custo de alimentação
  custo_alimentacao <-
    dados_banco %>%
    select(response_id, perm) %>%
    mutate(
      preco_alimentacao_ref = dados_excel$preco_alimentacao[[2,3]],
      custo_alimentacao = as.numeric(perm) * preco_alimentacao_ref * 2.25)
  
  custo_passeio <- 
    dados_banco %>%
    select(response_id, passeio_data, publico) %>%
    unnest(cols = passeio_data, keep_empty = TRUE) %>%  # Mantém os dados detalhados
    left_join(dados_excel$passeio_preco, by = c("resposta_id", "resposta_nome", "publico")) %>%
    group_by(response_id) %>%
    nest() %>%
    rename(data_passeio = data) %>%
    mutate(custo_passeio = map_dbl(data_passeio, ~ sum(as.numeric(.x$preco_passeio))))
  
  # Cálculo de custo de tempo (integral e parcial)
  custo_tempo <- 
    dados_banco %>%
    select(response_id, renda, perm) %>%
    mutate(
      Rd = as.numeric(renda) / 20,  # Ajuste para o cálculo da renda
      custo_tempo_integral = (Rd / 4) * as.numeric(perm),  # Custo de tempo integral
      custo_tempo_parcial = (Rd / 4)/2)
  
  # Consolidando todos os custos
  result <- list(
    perguntas_feitas = bd$perguntas %>%
      select(pergunta_num, pergunta_id, pergunta_nome, pergunta_tipo, pergunta_publico) %>%
      unique() %>%
      filter(pergunta_id %in% c(
        '151230060', # público
        '162823740', # motivo
        '151230104', # passeios_T
        '151230074', # origem
        '151230078', # perm
        '151230076', # hosp
        '151230079', # meio
        '151230075', # n_pessoas
        '151230111', # cia
        '151230077', # gasto_previsto_solo
        '151230110', # gasto_previsto_familia
        '151230083', # renda_T
        '151426421', # renda_M
        '151499315')),  # passeios_M
    
    respostas = dados_banco[,c("response_id", "motivo_boto", "publico", "publico_motivo")] %>%
      left_join(custo_transporte, by = "response_id") %>%
      left_join(custo_hospedagem, by = c("response_id")) %>%
      left_join(custo_alimentacao, by = c("response_id", "perm")) %>%
      left_join(custo_passeio, by = "response_id") %>%
      left_join(custo_tempo, by = c("response_id", "perm"))
  )
  
  return(result)
}