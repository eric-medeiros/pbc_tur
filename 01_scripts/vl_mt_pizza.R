# Função para calcular estatísticas gerais do banco de dados de valoração
vl_mt_pizza <- function(dados_val) {
  require(dplyr)
  require(ggplot2)
  
  dados <- dados_val$respostas %>%
    group_by(publico_motivo) %>%
    count() %>% 
    arrange(desc(publico_motivo)) %>%
    ungroup() %>%
    mutate(
      prop = (n / sum(n)) * 100,
      ypos = cumsum(prop) - 0.5 * prop,
      label = paste0(publico_motivo, "\n", round(prop, 1), "%\n", "n = ", n))
  
  grafico <- 
    dados %>%
    ggplot(aes(x = 1, fill = publico_motivo, y = prop)) +
    geom_col(width = 1, color = "white") +
    scale_fill_manual(values = c("lightblue","lightgrey","blue4","blue")) +
    geom_text(aes(y = ypos, x = 1, label = label), color = "darkgray", size = 4) +
    theme_void() + 
    ggtitle("Moradores e Turistas entrevistados em Cananéia - SP") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    coord_polar("y", start = 0) +
    annotate("text", x = 0, y = 1, label = paste0("Total de ", sum(dados$n), "\n entrevistados"), color = "darkgray")
  
  
  ggsave(
      path = "03_results",
      filename = "pizza.pdf",
      plot = grafico)
  
  result <- list(
    grafico = grafico,
    dados = dados
    )
  
  return(result)
}