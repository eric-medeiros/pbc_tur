# Função para processar metadados de perguntas
bd_limpa_metadados <- function( pasta_output ) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  
  caminho_dados_brutos <- file.path(pasta_output, "SM_bruto")
  caminho_metadados_bruto <- list.files(caminho_dados_brutos, full.names = TRUE, pattern = "metadados")
  dados_metadados <- readRDS(caminho_metadados_bruto)
  
  # DEFININDO FUNÇÕES ----
  # Função para extrair dados comuns de uma pergunta
  proc_dados_comuns <- function(page, question) {
    tibble(
      pagina_id = page$position,
      pagina_nome = str_remove_all(page$title, "<.*?>|</.*?>"),
      pergunta_id = question$id,
      pergunta_nome = str_remove_all(question$headings[[1]]$heading, "<.*?>|</.*?>"),
      pergunta_tipo = question$family,
      pergunta_subtipo = question$subtype
    )
  }
  
  # Função para processar escolhas de uma pergunta
  proc_choices <- function(choices) {
    if (is.list(choices)) {
      map_dfr(choices, ~ {
        peso <- if (length(.x$weight) == 0) NA_character_ else as.character(.x$weight)
        tibble(
          resposta_id = .x$id,
          resposta_opcao = if_else(.x$text == "", NA_character_, str_remove_all(.x$text, "<.*?>|</.*?>")),
          resposta_peso = peso,
          resposta_row = NA_character_,
          resposta_slider = NA_character_
        )
      })
    } else {
      tibble(
        resposta_id = NA_character_,
        resposta_opcao = NA_character_,
        resposta_peso = NA_character_,
        resposta_row = NA_character_,
        resposta_slider = NA_character_
      )
    }
  }
  
  # Função para processar linhas de uma pergunta
  proc_rows <- function(rows) {
    if (is.list(rows)) {
      map_dfr(rows, ~ tibble(
        resposta_id = .x$id,
        resposta_opcao = NA_character_,
        resposta_peso = NA_character_,
        resposta_row = .x$text,
        resposta_slider = NA_character_
      ))
    } else {
      tibble(
        resposta_id = NA_character_,
        resposta_opcao = NA_character_,
        resposta_peso = NA_character_,
        resposta_row = NA_character_,
        resposta_slider = NA_character_
      )
    }
  }
  
  # Função para processar cada pergunta
  processar_pergunta <- function(page, question) {
    dados_comuns <- proc_dados_comuns(page, question)
    
    escolhas <- proc_choices(question$answers$choices)
    linhas <- proc_rows(question$answers$rows)
    
    respostas <- 
      bind_rows(escolhas, linhas) %>%
      bind_cols(dados_comuns)
    
    return(respostas)
  }
  
  # RODANDO FUNÇÕES ----
  # Processar todas as páginas e perguntas
  dados_perguntas <- map_dfr(dados_metadados$pages, function(page) {
    map_dfr(page$questions, function(question) {
      processar_pergunta(page, question)
    })
  })
  
  # Limpar e transformar os dados
  perguntas <- 
  dados_perguntas %>%
    mutate(
      publico = str_extract(pagina_nome, "^TC|^TF|^M"),
      tipo = case_when(
        pergunta_tipo == "open_ended" & pergunta_subtipo == "essay" ~ "aberta_descritiva",
        pergunta_tipo == "open_ended" & pergunta_subtipo == "multi" ~ "aberta_objetiva",
        pergunta_tipo == "open_ended" & pergunta_subtipo == "single" ~ "aberta_objetiva",
        pergunta_tipo == "single_choice" ~ "unica_escolha",
        pergunta_tipo == "multiple_choice" ~ "multipla_escolha",
        pergunta_tipo == "matrix" & pagina_id %in% c(9, 23, 34) ~ "escala_subtopico",
        pergunta_tipo == "matrix" ~ "escala_simples",
        pergunta_tipo == "presentation" ~ "escala_topico"
      ),
      resposta = coalesce(resposta_peso, resposta_row, resposta_opcao, resposta_slider)
    ) %>%
    select(
      pagina_id, pagina_nome, pergunta_id, pergunta_nome, pergunta_tipo = tipo,
      pergunta_publico = publico, resposta_id, resposta_nome = resposta
    ) %>%
    group_by(pagina_id, pagina_nome, pergunta_id, pergunta_nome, pergunta_tipo, pergunta_publico) %>%
    mutate(resposta_num = row_number()) %>%
    select(1:6, 9, 7, 8) %>%
    nest() %>%
    ungroup() %>%
    mutate(pergunta_num = row_number()) %>%
    select(1:2, 8, 3:7) %>%
    unnest(cols = data)
  
  return(perguntas)
}
