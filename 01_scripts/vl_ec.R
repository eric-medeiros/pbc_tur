vl_ec <- function(dados_val) {
  require(dplyr)
  require(tidyr)

  # 1. Preparação dos vários conjuntos de dados utilizados
  dados_tcm <- 
    dados_val$respostas %>%
    filter(publico == "T") %>%
    filter(publico_motivo == "Turistas com motivo") %>%
    mutate(custo_c_boto = custo_transporte + custo_hospedagem + custo_alimentacao + custo_passeio + custo_tempo_integral) %>%
    select(response_id, publico_motivo, custo_c_boto)
  
  dados_tsm <- 
    dados_val$respostas %>%
    filter(publico == "T") %>%
    filter(publico_motivo == "Turistas sem motivo") %>%
    mutate(custo_c_boto = custo_passeio + custo_tempo_parcial) %>%
    select(response_id, publico_motivo, custo_c_boto)
  
  dados_tcm_sna <-
    dados_tcm %>%
    na.omit() %>%
    ungroup() %>%
    arrange(desc(custo_c_boto)) %>%
    mutate("Obs" = row_number())
  
  dados_tsm_sna <-
    dados_tsm %>%
    na.omit() %>%
    ungroup() %>%
    arrange(desc(custo_c_boto)) %>%
    mutate("Obs" = row_number())

  
  # 2. Modelo linear de regressão por grupo  
  modelo_tcm_sna <- lm(data = dados_tcm_sna, "custo_c_boto ~ Obs")
  modelo_tsm_sna <- lm(data = dados_tsm_sna, "custo_c_boto ~ Obs")
  
  dados_tcm_sna <- dados_tcm_sna %>% mutate(pred = predict(modelo_tcm_sna), area = (pred - mean(custo_c_boto)) * 1)
  dados_tsm_sna <- dados_tsm_sna %>% mutate(pred = predict(modelo_tsm_sna), area = (pred - mean(custo_c_boto)) * 1)
  
  # 3. Cálculos dos excedentes parciais e totais
  
  n_ent <- dados_val$respostas %>% nrow
  n_m <-  dados_val$respostas %>% filter(publico == "M") %>% nrow()
  n_t <- dados_val$respostas %>% filter(publico == "T") %>% nrow()
  n_na <- dados_val$respostas %>% filter(is.na(publico)) %>% nrow()
  # Do dados_tcm ou dados_tsm
  n_tcm <- dados_tcm %>% nrow()
  n_tsm <- dados_tsm %>% nrow()
  P_cm <- (n_tcm / n_t) %>% round(2)
  P_sm <- (n_tsm / n_t) %>% round(2)
  # Do dados_tcm_sna ou dados_tsm_sna
  n_tcm_sna <- dados_tcm_sna %>% nrow()
  n_tsm_sna <- dados_tsm_sna %>% nrow()
  area_cm <- dados_tcm_sna %>% filter(pred > mean(custo_c_boto)) %>% pull(area) %>% sum()
  area_sm <- dados_tsm_sna %>% filter(pred > mean(custo_c_boto)) %>% pull(area) %>% sum()
  area_p_n_tcm <- (area_cm / n_tcm_sna) %>% round(2)
  area_p_n_tsm <- (area_sm / n_tsm_sna) %>% round(2)
  media_custo_c_boto_tcm <- dados_tcm_sna %>% pull(custo_c_boto) %>% mean() %>% round(2)
  media_custo_c_boto_tsm <- dados_tsm_sna %>% pull(custo_c_boto) %>% mean() %>% round(2)
  # Hard code
  vis_nac <- 37775
  vis_int <- 1077
  vis_tot <- vis_nac + vis_int
  # Cálculos dos excedentes
  EC_tcm <- area_p_n_tcm * P_cm * vis_tot
  EC_tsm <- area_p_n_tsm * P_sm * vis_tot
  EC_total <- EC_tcm + EC_tsm
  
  
  resumo <- 
    tibble(
      n_ent = n_ent,
      n_m = n_m,
      n_t = n_t,
      n_na = n_na,
      n_tcm = n_tcm,
      n_tsm = n_tsm,
      P_cm = P_cm,
      P_sm = P_sm,
      n_tcm_sna = n_tcm_sna,
      n_tsm_sna = n_tsm_sna,
      area_cm = area_cm,
      area_sm = area_sm,
      area_p_n_tcm = area_p_n_tcm,
      area_p_n_tsm = area_p_n_tsm,
      media_custo_c_boto_tcm = media_custo_c_boto_tcm,
      media_custo_c_boto_tsm = media_custo_c_boto_tsm,
      vis_nac = vis_nac,
      vis_int = vis_int,
      vis_tot = vis_tot,
      EC_tcm = EC_tcm,
      EC_tsm = EC_tsm,
      EC_total = EC_total
    )
  
  # Juntando os resultados
  result <- 
    list(
      dados = list(
        dados_tcm = dados_tcm,
        dados_tsm = dados_tsm,
        dados_tcm_sna = dados_tcm_sna,
        dados_tsm_sna = dados_tsm_sna),
      modelos = list(
        modelo_tcm_sna = modelo_tcm_sna,
        modelo_tsm_sna = modelo_tsm_sna),
      resumo = resumo
    )
  
  return(result)
}