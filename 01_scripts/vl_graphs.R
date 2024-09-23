vl_graphs <- function(dados_stats) {
  
  dados_val_tur_sna <- dados_stats$dados$dados_val_tur_sna

  g1 <- 
    dados_val_tur_sna %>%
    ggplot(aes(x = custo_c_boto, y = gasto_previsto)) +
    geom_point(aes(colour = motivo_boto)) +
    geom_smooth(aes(colour = motivo_boto), method = "lm", se = FALSE) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
    labs(
      x = "gasto previsto (R$)",
      y = "gasto calculado com boto (R$)",
      title = "Relação do gasto previsto com custo calculado") +
    theme_classic() +
    annotate("text", x = 3800, y = 1000, size = 4,
             label = "custo calculado com base em:") +
    annotate("text", x = 3800, y = 700, size = 2,
             label = "1-origem, 2-meio de transporte, 3-tipo de hospedagem, 4-alimentação,") +
    annotate("text", x = 3800, y = 400, size = 2,
             label = "5-dias de permanência, 6-passeios realizados, 7-valor do tempo e 8-motivo da viagem") +
    theme(plot.title = element_text(hjust = 0.5))
  
  g2 <- 
    dados_val_tur_sna %>%
    ungroup() %>%
    transmute(
      Turista = case_when(
        motivo_boto ~ "Com \n motivo",
        !motivo_boto ~ "Sem \n motivo"
      ),
      Custo = custo_c_boto
    ) %>%
    ggplot(aes(y = Custo, x = Turista, fill = Turista)) +
    geom_violin(color = "white", alpha = 0.2) +
    geom_jitter(shape = 1, position = position_jitter(0.05), alpha = 0.3) +
    geom_boxplot(width = 0.05, fill = "white", alpha = 0.5, outliers = FALSE) +  
    stat_summary(fun = mean, geom = "point", color = "cyan", size = 3, alpha = 0.4) +
    scale_y_log10() +
    scale_x_discrete(limits = c("Sem \n motivo", "Com \n motivo")) + 
    theme_classic() +
    theme(legend.position = "none")

  
  g3 <-  
    ggplot() +
    geom_point(data = dados_stats$dados$dados_linha_cm, aes(x = Obs, y = custo_c_boto), color = "blue") +
    geom_line(data = dados_stats$dados$dados_linha_cm, aes(x = Obs, y = predicted)) +  
    geom_hline(yintercept = dados_stats$media_custo_cm, linetype = "dashed", color = "red") +
    geom_ribbon(data = dados_stats$dados$dados_area_cm, aes(x = Obs, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.4) +
    annotate(
      "text",
      x = mean(dados_stats$dados$dados_linha_cm$Obs),
      y = max(dados_stats$dados$dados_linha_cm$custo_c_boto),
      label = paste0("y = ", round(coef(dados_stats$model_cm)[2], 2), "x + ", round(coef(dados_stats$model_cm)[1], 2)),
      color = "darkblue") +
    annotate(
      "text",
      x = (min(dados_stats$dados$dados_linha_cm$Obs) + mean(dados_stats$dados$dados_linha_cm$Obs))/2, 
      y = (dados_stats$media_custo_cm + min(dados_stats$dados$dados_linha_cm$custo_c_boto))/2, 
      label = paste0("EC = R$ ", round(dados_stats$area_total_cm, 2), " / ", dados_stats$n_tur_cm_sna, " visitantes \n EC = ", round(dados_stats$EC_cm, 2), " / visitante"), 
      parse = FALSE,
      color = "black") +
    labs(title = "Curva de demanda do Turista com motivo", y = "Custo (R$)", x = "Entrevista num") +
    theme_classic() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

  g4 <-  
    ggplot() +
    geom_point(data = dados_stats$dados$dados_linha_sm, aes(x = Obs, y = custo_c_boto), color = "blue") +
    geom_line(data = dados_stats$dados$dados_linha_sm, aes(x = Obs, y = predicted)) +  
    geom_hline(yintercept = dados_stats$media_custo_sm, linetype = "dashed", color = "red") +
    geom_ribbon(data = dados_stats$dados$dados_area_sm, aes(x = Obs, ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.4) +
    annotate(
      "text",
      x = mean(dados_stats$dados$dados_linha_sm$Obs),
      y = max(dados_stats$dados$dados_linha_sm$custo_c_boto),
      label = paste0("y = ", round(coef(dados_stats$model_sm)[2], 2), "x + ", round(coef(dados_stats$model_sm)[1], 2)),
      color = "darkblue") +
    annotate(
      "text",
      x = (min(dados_stats$dados$dados_linha_sm$Obs) + mean(dados_stats$dados$dados_linha_sm$Obs))/2, 
      y = (dados_stats$media_custo_sm + min(dados_stats$dados$dados_linha_sm$custo_c_boto))/2, 
      label = paste0("EC = R$ ", round(dados_stats$area_total_sm, 2), " / ", dados_stats$n_tur_sm_sna, " visitantes \n EC = ", round(dados_stats$EC_sm, 2), " / visitante"), 
      parse = FALSE,
      color = "black") +
    labs(title = "Curva de demanda do Turista sem motivo", y = "Custo (R$)", x = "Entrevista num") +
    theme_classic() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  result <- list(
    g1 = g1,
    g2 = g2,
    g3 = g3,
    g4 = g4
  )
  return(result)
}  
