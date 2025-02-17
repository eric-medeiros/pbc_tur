vl_gr_violin_previsao <- function(dados_stats) {
  library(ggplot2)
  library(dplyr)
  
  # Dados simulados (use seus dados reais)
  dados_graph_base <- dados_graph_base %>%
    mutate(motivo_label = ifelse(motivo_boto, "Com motivo", "Sem motivo"))
  
  # Ajustando a contagem e proporção para a pirâmide por UF
  dados_count <-
    dados_graph_base %>%
    group_by(municipio, uf, motivo_label) %>%
    count(motivo_label) %>%
    group_by(municipio, uf) %>%
    mutate(prop = ifelse(motivo_label == "Com motivo", n, -n)) %>%
    arrange(uf, -prop) %>%
    rowid_to_column("order")
  
  # Plotando o gráfico no estilo pirâmide
  ggplot(dados_count, aes(x = reorder(municipio, order), y = prop, fill = motivo_label)) +
    geom_bar(stat = "identity", width = 0.6, alpha = 0.8, position = "identity", color = "white") +
    coord_flip() +  # Inverte o eixo para estilo de pirâmide
    scale_y_continuous(labels = abs) +  # Exibe os valores absolutos (sem negativos)
    labs(
      title = "Distribuição de Turistas com e sem motivo por UF",
      y = "Frequência de Turistas",
      fill = "Motivo"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  
  library(ggplot2)
  library(dplyr)
  
  # Ajustando a contagem e proporção para a pirâmide
  dados_count <-
    dados_graph_base %>%
    group_by(municipio, uf, motivo_label) %>%
    count(motivo_label) %>%
    group_by(municipio, uf) %>%
    mutate(prop = ifelse(motivo_label == "Com motivo", n, -n)) %>%
    arrange(uf, -prop) %>%
    rowid_to_column("order")
  
  # Plotando o gráfico no estilo pirâmide
  ggplot(dados_count, aes(x = reorder(municipio, order), y = prop, fill = motivo_label)) +
    geom_bar(stat = "identity", width = 0.6, alpha = 0.8, position = "identity", color = "white") +
    coord_flip() +  # Inverte o eixo para estilo de pirâmide
    scale_y_continuous(labels = abs) +  # Exibe os valores absolutos (sem negativos)
    labs(
      title = "Distribuição de Turistas com e sem motivo por UF",
      y = "Frequência de Turistas",
      fill = "Motivo"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    # Adicionando as UFs como texto no eixo direito
    geom_text(aes(y = 0, label = uf), 
              data = dados_count, 
              hjust = -0.1, size = 3)  # Ajuste do hjust para posicionamento
  
  
  
  
  library(ggplot2)
  library(dplyr)
  
  # Ajustando a contagem e proporção para a pirâmide
  dados_count <-
    dados_graph_base %>%
    group_by(municipio, uf, motivo_label) %>%
    count(motivo_label) %>%
    group_by(municipio, uf) %>%
    mutate(prop = ifelse(motivo_label == "Com motivo", n, -n)) %>%
    group_by(uf) %>%
    arrange(prop,.by_group = TRUE) %>%
    rowid_to_column("order")
  
  # Criar dados para as UFs
  uf_rects <- dados_count %>%
    group_by(uf) %>%
    summarize(ymax = max(prop) + 1,  # Ajuste para posição do retângulo
              ymin = min(prop) - 1,  # Ajuste para a parte inferior do retângulo
              municipio = unique(municipio)[1])  # Usar um município representativo
  
  # Plotando o gráfico no estilo pirâmide
  ggplot(dados_count, aes(x = reorder(municipio, order), y = prop, fill = motivo_label)) +
    geom_bar(stat = "identity", width = 0.6, alpha = 0.8, position = "identity", color = "white") +
    coord_flip() +  # Inverte o eixo para estilo de pirâmide
    scale_y_continuous(labels = abs) +  # Exibe os valores absolutos (sem negativos)
    labs(
      title = "Distribuição de Turistas com e sem motivo por UF",
      y = "Frequência de Turistas",
      fill = "Motivo"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    # Adicionando retângulos representando as UFs
    geom_rect(data = uf_rects, aes(xmin = -0.5, xmax = 0.5, ymin = ymin, ymax = ymax), 
              fill = "lightblue", alpha = 0.2) +
    geom_text(data = uf_rects, aes(x = 0, y = (ymax + ymin) / 2, label = uf), 
              size = 4, hjust = 0.5)  # Texto para as UFs
  
  
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Ajustando os dados para cada linha ser uma cidade
  dados_count <- 
    dados_graph_base %>%
    mutate(motivo_label = ifelse(motivo_boto, "Com motivo", "Sem motivo")) %>%
    group_by(municipio, uf, motivo_label) %>%
    count() %>%
    pivot_wider(names_from = motivo_label, values_from = n, values_fill = 0) %>%
    mutate(prop_com_motivo = `Com motivo`, 
           prop_sem_motivo = -`Sem motivo`) %>%
    arrange(uf, desc(prop_com_motivo))
  
  # Plotando o gráfico no estilo pirâmide
  ggplot(dados_count, aes(x = reorder(municipio, prop_com_motivo), fill = "Com motivo")) +
    geom_bar(aes(y = prop_com_motivo), stat = "identity", width = 0.6, alpha = 0.8, color = "white") +
    geom_bar(aes(y = prop_sem_motivo), stat = "identity", width = 0.6, alpha = 0.8, fill = "orange", color = "white") +
    coord_flip() +  # Inverte o eixo para estilo de pirâmide
    scale_y_continuous(labels = abs) +  # Exibe os valores absolutos (sem negativos)
    labs(
      title = "Distribuição de Turistas com e sem motivo por Município",
      y = "Frequência de Turistas",
      fill = "Motivo"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    # Adicionando retângulos representando as UFs
    geom_rect(data = dados_count %>% 
                group_by(uf) %>% 
                summarize(ymax = max(prop_com_motivo), 
                          ymin = min(prop_sem_motivo), 
                          .groups = 'drop'), 
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
              fill = "lightblue", alpha = 0.2) +
    # Texto para as UFs
    geom_text(data = dados_count %>% 
                group_by(uf) %>% 
                summarize(y = mean(prop_com_motivo), 
                          uf = unique(uf), .groups = 'drop'), 
              aes(x = -0.5, y = y, label = uf), 
              size = 4, hjust = 1)  # Ajustando a posição do texto para as UFs

  library(ggplot2)
  library(dplyr)

  library(ggplot2)
  library(dplyr)
  
  # Contando o número de ocorrências por UF
  dados_count_uf <- dados_graph_base %>%
    group_by(uf) %>%
    count(name = "n") %>%
    arrange(n)
  
  # Computando frações e posições
  dados_count_uf$fraction <- dados_count_uf$n / sum(dados_count_uf$n)
  dados_count_uf$ymax <- cumsum(dados_count_uf$fraction)
  dados_count_uf$ymin <- c(0, head(dados_count_uf$ymax, n = -1))
  dados_count_uf$labelPosition <- (dados_count_uf$ymax + dados_count_uf$ymin) / 2
  dados_count_uf$label <- paste0(dados_count_uf$uf, "\nvalue: ", dados_count_uf$n)
  
  # Criando o gráfico de pizza
  ggplot(dados_count_uf, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = uf)) +
    geom_rect() +
    geom_label(x = 4.1, aes(y = labelPosition, label = label), size = 3) +
    scale_x_continuous(limits = c(2,5)) +
    scale_fill_brewer(palette = "Set3") +  # Use a paleta de cores desejada
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Número de Ocorrências por Unidade Federativa (UF)")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  media <-
    dados_graph_base %>%
    group_by(Turista) %>%
    summarise(media = mean(Previsao, na.rm = TRUE) %>% round(2))
  
  # Testes de normalidade
  shapiro_com <- shapiro.test(dados_graph_base$Previsao[dados_graph_base$Turista == "Com \n motivo"])
  shapiro_sem <- shapiro.test(dados_graph_base$Previsao[dados_graph_base$Turista == "Sem \n motivo"])
  
  # Escolha do teste de comparação com base na normalidade
  if (shapiro_com$p.value > 0.05 & shapiro_sem$p.value > 0.05) {
    teste <- t.test(Previsao ~ Turista, data = dados_graph_base)
    tipo_teste <- "T-Test"
  } else {
    teste <- wilcox.test(Previsao ~ Turista, data = dados_graph_base, exact = FALSE)  # Mann-Whitney U
    tipo_teste <- "Mann-Whitney"
  }
  
  # Ajustando a formatação do valor de p
  resumo_shapiro_com <- paste0("Shapiro p = ", ifelse(shapiro_com$p.value < 0.001, "<0.001", formatC(shapiro_com$p.value, format = "f", digits = 4)))
  resumo_shapiro_sem <- paste0("Shapiro p = ", ifelse(shapiro_sem$p.value < 0.001, "<0.001", formatC(shapiro_sem$p.value, format = "f", digits = 4)))
  
  # Resumo do teste Mann-Whitney/T-Test
  resumo_teste <- paste0(
    tipo_teste, ": p = ", ifelse(teste$p.value < 0.001, "<0.001", formatC(teste$p.value, format = "f", digits = 4)), "\n",
    "Diferença: ", ifelse(teste$p.value < 0.05, "Sim", "Não")
  )
  
  # Criar gráfico com as anotações
  dados_graph_base %>%
    ggplot(aes(y = Previsao, x = Turista, fill = Turista)) +
    geom_violin(color = "white", alpha = 0.2) +
    geom_jitter(shape = 1, position = position_jitter(0.05), alpha = 0.3) +
    geom_boxplot(width = 0.05, fill = "white", alpha = 0.5, outliers = FALSE) +
    geom_point(data = media, aes(y = media, x = Turista), color = "blue", size = 2, alpha = 0.6) +
    # Anotar médias calculadas manualmente
    annotate("text", x = 0.75, y = media$media[1], label = paste0("Média:\nR$ ", media[1,2]), color = "blue", size = 4) +
    annotate("text", x = 2.25, y = media$media[2], label = paste0("Média:\nR$ ", media[2,2]), color = "blue", size = 4) +
    
    # Anotações de Shapiro-Wilk
    annotate("rect", xmin = 0.75, xmax = 1.25, ymin = max(dados_graph_base$Previsao)*1.2, ymax = max(dados_graph_base$Previsao)*1.55, alpha = 0.1, fill = "black") +
    annotate("text", x = 1, y = max(dados_graph_base$Previsao) * 1.4, label = resumo_shapiro_sem, hjust = 0.5, size = 4, color = "black") +
    annotate("rect", xmin = 1.75, xmax = 2.25, ymin = max(dados_graph_base$Previsao)*1.2, ymax = max(dados_graph_base$Previsao)*1.55, alpha = 0.1, fill = "black") +
    annotate("text", x = 2, y = max(dados_graph_base$Previsao) * 1.4, label = resumo_shapiro_com, hjust = 0.5, size = 4, color = "black") +
    
    # Anotação do teste Mann-Whitney/T-Test no centro
    annotate("rect", xmin = 1.1, xmax = 1.9, ymin = min(dados_graph_base$Previsao)-15, ymax = min(dados_graph_base$Previsao)+60, alpha = 0.1, fill = "black") +
    annotate("text", x = 1.5, y = min(dados_graph_base$Previsao)+20, label = resumo_teste, hjust = 0.5, size = 4) + 
    scale_y_log10(limits = c(min(dados_graph_base$Previsao)-15, max(dados_graph_base$Previsao) * 1.7)) +
    scale_x_discrete(limits = c("Sem \n motivo", "Com \n motivo")) +
    theme_classic() +
    labs(title = "Previsão de gastos informada", y = "Previsão (R$)") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
}