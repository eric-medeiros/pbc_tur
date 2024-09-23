vl_dados <- function(pasta_proj) {
  # Carregar pacotes
  library(dplyr)
  library(tidyr)
  library(purrr)
  
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
  
  perguntas_fetias <- 
    bd$perguntas %>%
    select(pergunta_num, pergunta_id, pergunta_nome, pergunta_tipo, pergunta_publico) %>%
    unique() %>%
    filter(
      pergunta_id %in% c(
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
        '151499315'  # passeios_M
      ))
  
  # Gasto previsto
  gasto_previsto <- 
    dados_banco %>%
    mutate(gasto_previsto = round_if_numeric(gasto)) %>%
    select(response_id, gasto_previsto)
  
  # Cálculo de custo de transporte
  custo_transporte <- 
    dados_banco %>%
    select(response_id, meio, motivo_boto) %>%
    left_join(dados_excel$resposta_origem, by = "response_id") %>%
    left_join(
      dados_excel$ciadade_preco %>%
        pivot_longer(3:10, names_to = "meio", values_to = "custo_transporte", values_drop_na = TRUE),
      by = c("municipio", "uf", "meio")
    ) %>%
    mutate(custo_transporte = case_when(
      motivo_boto == TRUE ~ round_if_numeric(custo_transporte),
      TRUE ~ NA_real_
    )) %>%
    select(-resposta)
  
  # Cálculo de custo de hospedagem
  custo_hospedagem <- 
    dados_banco %>%
    select(response_id, hospedagem, perm, motivo_boto) %>%
    left_join(dados_excel$hospedagem_diaria, by = "hospedagem") %>%
    mutate(custo_hospedagem = case_when(
      motivo_boto == TRUE ~ round_if_numeric(as.numeric(valor) * as.numeric(perm))
    ))
  
  # Cálculo de custo de alimentação - ARRUMAR ALIMENTAÇÃO AQUI
  custo_alimentacao <-
    dados_banco %>%
    select(response_id, perm, motivo_boto) %>%
    mutate(custo_alimentacao = case_when(
      motivo_boto == TRUE ~ round_if_numeric(as.numeric(perm) * 0)
    ))
  
  # Cálculo de custo de passeio
  custo_passeio <- 
    dados_banco %>%
    select(response_id, passeio_data, publico, motivo_boto) %>%
    unnest(cols = passeio_data, keep_empty = TRUE) %>%
    left_join(dados_excel$passeio_preco, by = c("resposta_id", "resposta_nome", "publico")) %>%
    group_by(response_id, motivo_boto) %>%
    summarise(
      custo_passeio = sum(as.numeric(preco)) %>% round_if_numeric(), 
      .groups = "drop"
    )
  
  # Cálculo de custo de tempo
  custo_tempo <- 
    dados_banco %>%
    select(response_id, publico, renda, perm, motivo_boto) %>%
    mutate(
      Rd = as.numeric(renda) / 20,
      custo_tempo = case_when(
        motivo_boto == TRUE ~ round_if_numeric((Rd / 4) * as.numeric(perm)),
        motivo_boto == FALSE ~ round_if_numeric((Rd / 4) / 2)
      )
    )
  
  # Consolidar resultados
  result <-
    list(
      base = list(
        dados_banco = dados_banco,
        dados_excel = dados_excel,
        perguntas_fetias = perguntas_fetias
      ),
      interm = list(
        gasto_previsto = gasto_previsto,
        custo_transporte = custo_transporte,
        custo_hospedagem = custo_hospedagem,
        custo_alimentacao = custo_alimentacao,
        custo_passeio = custo_passeio,
        custo_tempo = custo_tempo
      ))
  
  # 1- Dados da valoração
  result$dados_val <- 
    result$interm %>%
    reduce(left_join, by = "response_id") %>%
    select(
      response_id,
      publico,
      motivo_boto = motivo_boto.x,
      custo_transporte,
      custo_hospedagem,
      custo_alimentacao,
      custo_passeio,
      custo_tempo,
      gasto_previsto
    ) %>%
    mutate(across(custo_transporte:custo_tempo, round_if_numeric)) %>%
    rowwise() %>%
    mutate(custo_c_boto = sum(c_across(custo_transporte:custo_tempo), na.rm = TRUE)) %>%
    group_by(response_id)
  
  # 2- Dados da valoração de turistas
  result$dados_val_tur <- 
    result$dados_val %>%
    filter(publico == "T")
  
  # 3- Dados da valoração de turistas com motivo sem NAs
  result$dados_val_tur_cm_sna <- 
    result$dados_val_tur %>%
    filter(motivo_boto) %>%
    drop_na() %>%
    arrange(-custo_c_boto) %>%
    ungroup() %>%
    mutate(Obs = row_number())
  
  # 4- Dados da valoração de turistas sem motivo sem NAs
  result$dados_val_tur_sm_sna <- 
    result$dados_val_tur %>%
    filter(!motivo_boto) %>%
    drop_na(-custo_transporte, -custo_hospedagem, -custo_alimentacao) %>%
    arrange(-custo_c_boto) %>%
    ungroup() %>%
    mutate(Obs = row_number())
  
  # 5- Dados da valoração de turistas sem NAs
  result$dados_val_tur_sna <- 
    bind_rows(result$dados_val_tur_cm_sna, result$dados_val_tur_sm_sna)
  
  return(result)
}