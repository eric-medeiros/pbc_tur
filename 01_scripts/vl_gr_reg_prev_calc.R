vl_gr_reg_prev_calc <- function(dados_stats) {
  # fz analises aqui dentro e qm sabe retornar tabela de infos stats?
  dados_val_tur_sna <- dados_stats$dados$dados_val_tur_sna
  
  g <- 
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
  
  return(g)
}