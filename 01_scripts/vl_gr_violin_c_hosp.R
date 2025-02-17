vl_gr_violin_c_hosp <- function(dados_stats) {
  # Carregar pacotes necessários
  library(ggplot2)
  library(dplyr)
  library(broom)
  
  faz_gr_hosp <- function(dados_graph_hosp){
    media <-
      dados_graph_hosp %>%
      group_by(Turista) %>%
      summarise(media = mean(custo_hospedagem, na.rm = TRUE) %>% round(2))
    
    # Testes de normalidade
    shapiro_com <- shapiro.test(dados_graph_hosp$custo_hospedagem[dados_graph_hosp$Turista == "Com \n motivo"])
    shapiro_sem <- shapiro.test(dados_graph_hosp$custo_hospedagem[dados_graph_hosp$Turista == "Sem \n motivo"])
    
    # Escolha do teste de comparação com base na normalidade
    if (shapiro_com$p.value > 0.05 & shapiro_sem$p.value > 0.05) {
      teste <- t.test(custo_hospedagem ~ Turista, data = dados_graph_hosp)
      tipo_teste <- "T-Test"
    } else {
      teste <- wilcox.test(custo_hospedagem ~ Turista, data = dados_graph_hosp, exact = FALSE)  # Mann-Whitney U
      tipo_teste <- "Mann-Whitney"
    }
    
    label_shap_com <- 
      list(
        label = paste0(
          "n = ", dados_graph_hosp %>% filter(Turista == "Com \n motivo") %>% nrow(), "\n",
          "Shapiro p = ", ifelse(shapiro_com$p.value < 0.001, "<0.001", formatC(shapiro_com$p.value, format = "f", digits = 4)), "\n",
          "Normalidade: ", ifelse(shapiro_com$p.value < 0.05, "Não", "Sim")),
        y = max(dados_graph_hosp$custo_hospedagem, na.rm = TRUE) * 1.4
      )
    
    label_shap_sem <- 
      list(
        label = paste0(
          "n = ", dados_graph_hosp %>% filter(Turista == "Sem \n motivo") %>% nrow(), "\n",
          "Shapiro p = ", ifelse(shapiro_sem$p.value < 0.001, "<0.001", formatC(shapiro_sem$p.value, format = "f", digits = 4)), "\n",
          "Normalidade: ", ifelse(shapiro_sem$p.value < 0.05, "Não", "Sim")),
        y = max(dados_graph_hosp$custo_hospedagem, na.rm = TRUE) * 1.4
      )
    
    label_media_com <- list(
      label = paste0( "Média:\nR$ ", media %>% filter(Turista == "Com \n motivo") %>% pull(media)),
      y = media %>% filter(Turista == "Com \n motivo") %>% pull(media)
    )
    
    label_media_sem <- list(
      label = paste0( "Média:\nR$ ", media %>% filter(Turista == "Sem \n motivo") %>% pull(media)),
      y = media %>% filter(Turista == "Sem \n motivo") %>% pull(media)
    )
    
    label_dif_test <- list(
      label = paste0(
        tipo_teste, ": p = ", ifelse(teste$p.value < 0.001, "<0.001", formatC(teste$p.value, format = "f", digits = 4)), "\n",
        "Diferença: ", ifelse(teste$p.value < 0.05, "Sim", "Não")),
      y = min(dados_graph_hosp$custo_hospedagem + 60, na.rm = TRUE)
    )
    
    # Criar gráfico com as anotações
    g <- 
      dados_graph_hosp %>%
      ggplot(aes(y = custo_hospedagem, x = Turista, fill = Turista)) +
      # Importante mexer no eixo já pela ordem de cosntrução do graph
      scale_y_log10(limits = c(min(dados_graph_hosp$custo_hospedagem)-15, max(dados_graph_hosp$custo_hospedagem) * 1.7)) +
      # geoms
      geom_violin(color = "white", alpha = 0.2) +
      geom_jitter(shape = 1, position = position_jitter(0.05), alpha = 0.3) +
      geom_boxplot(width = 0.05, fill = "white", alpha = 0.5, outliers = FALSE) +
      geom_point(data = media, aes(y = media, x = Turista), color = "blue", size = 2, alpha = 0.6) +
      # Anotações das médias - sinc com a troca do eixo
      scale_x_discrete(limits = c("Sem \n motivo", "Com \n motivo")) +
      annotate("text", x = 2.2, y = label_media_com$y, label = label_media_com$label, color = "blue", size = 3) +
      annotate("text", x = 0.8, y = label_media_sem$y, label = label_media_sem$label, color = "blue", size = 3) +
      # Anotações de Shapiro-Wilk
      annotate("text", x = 1, y = label_shap_sem$y, label = label_shap_sem$label, hjust = 0.5, size = 3, color = "gray") +
      annotate("text", x = 2, y = label_shap_com$y, label = label_shap_com$label, hjust = 0.5, size = 3, color = "gray") +
      # Anotação do teste Mann-Whitney/T-Test no centro
      annotate("text", x = 1.5, y = label_dif_test$y, label = label_dif_test$label, hjust = 0.5, size = 4) + 
      # Finalização
      theme_classic() +
      labs(title = "Custo médio de hospedagem calculado em Cananéia - SP", y = "Custo (R$)") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
    return(g)
  }
  
  # Coletar os dados necessários
  dados_graph_base <- 
    dados_stats$dados$dados_custos %>%
    reduce(left_join, by = "response_id") %>%
    select(
      response_id,
      motivo_boto,
      custo_hospedagem
    ) %>%
    filter(response_id %in% dados_stats$dados$dados_val_tur_sna$response_id) %>%
    transmute(
      Turista = case_when(
        motivo_boto ~ "Com \n motivo",
        !motivo_boto ~ "Sem \n motivo"
      ),
      custo_hospedagem = custo_hospedagem
    )
  
  g_fil <- 
    dados_graph_base %>%
    filter(custo_hospedagem > 0) %>%
    faz_gr_hosp()
  
  g_base <- 
    dados_graph_base %>%
    faz_gr_hosp()
  
  result <- list(
    g_fil = g_fil,
    g_base = g_base
  )
  
  return(result)
}