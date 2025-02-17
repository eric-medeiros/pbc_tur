vl_gr_ec_cm <- function (dados_stats) {
  # fz analises aqui dentro e qm sabe retornar tabela de infos stats?
  dados_val_tur_sna <- dados_stats$dados$dados_val_tur_sna
  
  g <-  
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
  
  return(g)
}