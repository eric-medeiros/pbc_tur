rd_trata_renda <- function(dados_renda) {
  
  dados_dentro <- # Acho que só vai pra gafico, se sim, jogar na função gráfico
    dados_renda %>%
    mutate(
      respondeu = case_match(
        resposta,
        "0 a 1.320 (Até 1 salário Mínimo)" ~ "validado",
        "1.321 a 2.640 (de 1 a 2 salários mínimos)" ~ "validado",
        "2.641 a 6.600 (de 2 a 5 salários mínimos)" ~ "validado",
        "6.601 a 13.200 (de 5 a 10 salários mínimos)" ~ "validado",
        "13.201+ (mais de 10 salários mínimos)" ~ "validado",
        "Desejo não informar" ~ "invalidado",
        NA ~ "invalidado"
      ),
      respondeu = factor(respondeu, levels = c("validado", "invalidado"))
    ) %>%
    count(respondeu) %>%
    mutate(
      perc = n / sum(n),
      ymax = cumsum(perc),
      ymin = lag(ymax, default = 0),
      y = (ymax + ymin) / 2
    )
  
  dados_fora <- 
    dados_renda %>%
    mutate(
      resposta = factor(
        resposta,
        levels = c(
          "0 a 1.320 (Até 1 salário Mínimo)",
          "1.321 a 2.640 (de 1 a 2 salários mínimos)",
          "2.641 a 6.600 (de 2 a 5 salários mínimos)",
          "6.601 a 13.200 (de 5 a 10 salários mínimos)",
          "13.201+ (mais de 10 sal\u00e1rios mínimos)",
          "Desejo n\u00e3o informar"
        )
      )
    ) %>%
    count(resposta, .drop = FALSE) %>%
    mutate(
      perc = n / sum(n),
      ymax = cumsum(perc),
      ymin = lag(ymax, default = 0),
      y = (ymax + ymin) / 2
    )
  
  dados_filtrados <-
    dados_fora %>%
    filter(resposta != "Desejo não informar")
  
  dados_tab <- 
    dados_filtrados %>%
    mutate(perc = scales::percent(perc)) %>%
    mutate(
      renda_min = c(0, 1321, 2641, 6601, 13201),
      renda_max_true = c(1321, 2641, 6601, 13201, NA),
      renda_max_fake = c(1321, 2641, 6601, 13201, 13201),
      renda_media_true = as.integer((renda_min + renda_max_true) / 2),
      renda_media_fake = as.integer((renda_min + renda_max_fake) / 2)
    ) %>%
    ungroup() %>%
    select(renda_min, renda_max_true, renda_max_fake, n, perc, renda_media_true, renda_media_fake)
  
  dados_media_renda <- 
    dados_tab %>%
    summarise(
      media_true = weighted.mean(renda_media_true, n, na.rm = TRUE) %>% round(2),
      n_true = sum(n) - dados_tab[[5,"n"]],
      media_fake = weighted.mean(renda_media_fake, n, na.rm = TRUE) %>% round(2),
      n_fake = sum(n)
    )
  
  result <- list(
    dados_dentro = dados_dentro,
    dados_fora = dados_fora,
    dados_filtrados = dados_filtrados,
    dados_tab = dados_tab,
    dados_media_renda = dados_media_renda
  )
  
    return(result)
}
