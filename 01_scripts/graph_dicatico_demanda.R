# Carregar as bibliotecas necessárias
library(ggplot2)
library(dplyr)

# Gerar dados fictícios seguindo uma tendência de demanda
set.seed(123)
n <- 50
Visita <- 1:n  # Número da visita
Custo_previsto <- 2000 - 30 * Visita + rnorm(n, sd = 100)  # Relação linear decrescente (curva de demanda)

dados <- data.frame(
  Visita = Visita,
  Custo_previsto = Custo_previsto
)

# Calcular a média do custo previsto
media_custo <- mean(dados$Custo_previsto)

# Ajustar o modelo linear (reta de regressão)
modelo <- lm(Custo_previsto ~ Visita, data = dados)
coeficientes <- coef(modelo)

# Criar pontos para o triângulo (área entre a reta e a média)
dados_triangulo <- data.frame(
  x = c(min(dados$Visita), min(dados$Visita), median(dados$Visita)),
  y = c(predict(modelo, newdata = data.frame(Visita = min(dados$Visita))),
        media_custo,
        media_custo)
)

# Criar o gráfico com ggplot2
ggplot(dados, aes(x = Visita, y = Custo_previsto)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linewidth = 1.2) +
  geom_hline(yintercept = media_custo, linetype = "dashed", color = "red", size = 1) +
  geom_polygon(data = dados_triangulo, aes(x = x, y = y), fill = "lightgray", alpha = 0.5) +
  annotate(
    "text", x = 15, y = 2000, 
    label = "1 - Pontos dos \ndados", color = "blue", size = 4, hjust = 0.5) +
  annotate(
    "text", x = 20, y = 1700, 
    label = "2 - Reta de regressão: y = a + bx", color = "darkgreen", size = 4, hjust = 0) +
  annotate(
    "text", x = 30, y = media_custo + 100, 
    label = "3 - Média do custo", color = "red", size = 4, hjust = 0) +
  annotate(
    "text", x = n / 4, y = media_custo - 150, 
    label = "4 - Área do triângulo:\n1/2 * base * altura", 
    color = "black", size = 4, hjust = 0.5) +
  labs(
    title = "Curva de Demanda: Custo Previsto vs. Número da Turistas",
    x = "Turista (número)",
    y = "Custo Previsto (R$)" ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centralizar o título
    axis.text.y = element_blank(),  # Remover os valores do eixo Y
    axis.text.x = element_blank()   # Remover os valores do eixo X
  )
