vl_gr_violin_c_total <- function(dados_stats) {
  # fz analises aqui dentro e qm sabe retornar tabela de infos stats?
  dados_val_tur_sna <- dados_stats$dados$dados_val_tur_sna
  
  g <- 
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
  
  return(g)
}