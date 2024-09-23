# Função para calcular estatísticas gerais do banco de dados de valoração
vl_stats <- function(dados_val) {
  # Carregando pacotes necessários
  library(dplyr)

  # BASE
  # Calculando totais de público
  n_ent <- dados_val$dados_val %>% nrow()
  n_mor <- dados_val$dados_val %>% filter(publico == "M") %>% nrow()
  n_tur <- dados_val$dados_val %>% filter(publico == "T") %>% nrow()
  n_na <- dados_val$dados_val %>% filter(is.na(publico)) %>% nrow()
  # Contando os turistas por c/s motivo
  n_tur_cm <- dados_val$dados_val_tur %>% filter(motivo_boto) %>% nrow()
  n_tur_sm <- dados_val$dados_val_tur %>% filter(!motivo_boto) %>% nrow()
  # Contando os turistas por c/s motivo sem NA
  n_tur_cm_sna <- dados_val$dados_val_tur_cm_sna %>% nrow()
  n_tur_sm_sna <- dados_val$dados_val_tur_sm_sna %>% nrow()
  # Proporção de turistas com e sem motivo para turistas
  P_cm <- n_tur_cm/n_tur
  P_sm <- n_tur_sm/n_tur
  
  # HARDCODE
  n_nac <- 37775
  n_int <- 1077
  n_tot <- n_nac + n_int
  
  # ÁREA E EC de COM E SEM
  # Modelo de regressão linear
  model_cm <- lm(custo_c_boto ~ Obs, data = dados_val$dados_val_tur_cm_sna)
  model_sm <- lm(custo_c_boto ~ Obs, data = dados_val$dados_val_tur_sm_sna)
  # Previsão dos dados com base no modelo
  dados_val$dados_val_tur_cm_sna$predicted <- predict(model_cm, newdata = dados_val$dados_val_tur_cm_sna)
  dados_val$dados_val_tur_sm_sna$predicted <- predict(model_sm, newdata = dados_val$dados_val_tur_sm_sna)
  # Média dos custos com boto
  media_custo_cm <- mean(dados_val$dados_val_tur_cm_sna$custo_c_boto)
  media_custo_sm <- mean(dados_val$dados_val_tur_sm_sna$custo_c_boto)
  # Áreas parciais do triangulo
  dados_linha_cm <- 
    dados_val$dados_val_tur_cm_sna %>%
    mutate(ymin = media_custo_cm, ymax = predicted)
  dados_area_cm <- 
    dados_linha_cm %>%
    filter(predicted > media_custo_cm) %>%
    arrange(Obs) %>%
    mutate(area = (predicted - media_custo_cm) * (Obs - lag(Obs, default = first(Obs))))
  dados_linha_sm <- 
    dados_val$dados_val_tur_sm_sna %>%
    mutate(ymin = media_custo_sm, ymax = predicted)
  dados_area_sm <- 
    dados_linha_sm %>%
    filter(predicted > media_custo_sm) %>%
    arrange(Obs) %>%
    mutate(area = (predicted - media_custo_sm) * (Obs - lag(Obs, default = first(Obs))))
  # Áreas totais
  area_total_cm <- sum(dados_area_cm$area, na.rm = TRUE)
  area_total_sm <- sum(dados_area_sm$area, na.rm = TRUE)
  # EC = A / num de pessoas
  EC_cm <- area_total_cm / n_tur_cm_sna
  EC_sm <- area_total_sm / n_tur_sm_sna
  
  #CALCULO DO EC TOTAL!!!
  EC_total <- (EC_cm*P_cm*n_tot)+(EC_sm*P_sm*n_tot)
  
  result <-  
    list(
      n_ent = n_ent,
      n_mor = n_mor,
      n_tur = n_tur,
      n_na = n_na,
      n_tur_cm = n_tur_cm,
      n_tur_sm = n_tur_sm,
      n_tur_cm_sna = n_tur_cm_sna,
      n_tur_sm_sna = n_tur_sm_sna,
      n_nac = n_nac,
      n_int = n_int,
      n_tot = n_tot,
      P_cm = P_cm,
      P_sm = P_sm,
      model_cm = model_cm,
      model_sm = model_sm,
      media_custo_cm = media_custo_cm,
      media_custo_sm = media_custo_sm,
      area_total_cm = area_total_cm,
      area_total_sm = area_total_sm,
      EC_cm = EC_cm,
      EC_sm = EC_sm,
      EC_total = EC_total,
      dados = list(
        dados_val = dados_val$dados_val,
        dados_val_tur = dados_val$dados_val_tur,
        dados_val_tur_sna = dados_val$dados_val_tur_sna,
        dados_val_tur_cm_sna = dados_val$dados_val_tur_cm_sna,
        dados_val_tur_sm_sna = dados_val$dados_val_tur_sm_sna,
        dados_linha_cm = dados_linha_cm,
        dados_linha_sm = dados_linha_sm,
        dados_area_cm = dados_area_cm,
        dados_area_sm = dados_area_sm
      )
    )
  
  return(result)  
}
