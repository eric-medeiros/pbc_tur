library(svmkR)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(lubridate)


library(ggplot2)
# library(unpivotr)
# library(wrapr)
# library(forcats)

# Para procurar as surveys disponíveis
# surveys <- browse_surveys(10)

# Perguntas - bruto
metadados <- fetch_survey_obj(514390303)

# Perguntas - fino
perguntas <- metadados$pages %>%
  map_dfr(~{
    pagina_id <- .x$position
    pagina <- .x$title %>% str_remove_all("<.*?>|</.*?>")
    questions <- .x$questions %>%
      map_dfr(~{
        pergunta_id <- .x$id
        pergunta <- .x$headings[[1]]$heading %>% str_remove_all("<.*?>|</.*?>")
        tipo <- .x$family
        subtipo <- .x$subtype
        choices <- .x$answers$choices %>%
          map_df(~tibble(
            resposta_id = .x$id,
            resposta = .x$text %>% str_remove_all("<.*?>|</.*?>"),
            peso = .x$weight
          ))
        tibble(pagina_id, pagina, pergunta_id, pergunta, tipo, subtipo, choices)
      })
    questions
  }) %>%
  mutate(pergunta_num = case_when(row_number(pergunta_id) == 1 ~ 1)) %>%
  rowwise() %>%
  mutate(pergunta_num = case_when(lag(pergunta_num) == 1 ~ lag(pergunta_num)))
,
      row_number(pergunta_id) != 1 & pergunta_id == lag(pergunta_id) ~ lag(pergunta_num))) %>% View()
  nest_by(pagina_id, pergunta_id) %>%
  mutate(pergunta_num = cur_group_id()) %>%
  mutate()


  nest_by(pagina_id, pergunta_id) %>%
  ungroup() %>%
  unnest(cols = c(data)) %>%
  group_by(pergunta_num) %>%
  mutate(resposta_num = paste0("R", row_number()))

# Collectors sem teste, 
collectors <- 
  read_excel("data/CollectorList.xlsx") %>%
  filter(
    Locale %in% c( # Pegando apenas coletores usados
      "CIT",
      "Balsa Continente",
      "Balsa Ilha Comprida",
      "Avenida Beira-Mar",
      "Pereirinha"
    )) %>%
  mutate(Date_F = case_when(Date_F == "-" ~ Sys.Date() + 1,
                            TRUE ~ dmy(Date_F)),
         Date_I = dmy(Date_I)) %>%
  suppressWarnings()

# Respostas - your survey's ID goes here
survey_df <- 
  fetch_survey_obj(514390303) %>%
  parse_survey() %>% 
  select(
    -collection_mode,
    -survey_id,
    -first_name,
    -last_name,
    -email_address
  ) %>% 
  mutate(
    date_created = ymd_hms(date_created, tz = "America/Sao_Paulo"),
    date_modified = ymd_hms(date_modified, tz = "America/Sao_Paulo"),
    date = date(date_created)
  )

entrevistas <- 
  survey_df %>%
  select(
    response_id,
    collector_id,
    date,
    response_status,
    `151230059`,
    date_created,
    date_modified,
    ip_address
  ) %>%
  mutate(
    response_status = case_match(response_status,
                                 "completed" ~ TRUE,
                                 "partial" ~ FALSE),
    `151230059` = case_match(`151230059`,
                             "Eu li e concordo com o termo de consentimento e desejo continuar" ~ TRUE,
                             "Eu discordo e/ou não irei continuar" ~ FALSE)) %>%
  rename(
    completo = response_status,
    concorda = `151230059`)

survey_df %>%
  select(-c(collector_id, response_status, date_created, date_modified, date, ip_address, `151230059`)) %>%
  mutate(`151230060` = case_match(`151230060`,
                                  "Estou visitando Cananéia" ~ "TC",
                                  "Já visitei, mas não estou em Cananéia" ~ "TF",
                                  "Moro em Cananéia" ~ "M")) %>%
  rename(
    Perfil = `151230060`
  )
  colnames()
  rename(grupo = '151230060') %>%
  mutate_if(is.logical, as.character) %>%
  pivot_longer(cols = -c(1,2), names_to = "id", values_to = "resposta") %>%
  select(1:2, id, resposta) %>% View()




survey_df %>%
  select(-c(collector_id, response_status, date_created, date_modified, date, ip_address)) %>%
  pivot_longer(cols = as.character(4:19), names_to = "perguntas")




  inner_join(collectors[,c(1,4,5,6)], by = join_by(collector_id == CollectorID,
                                                   between(date_created, Date_I, Date_F))
  ) %>% as_cells() %>% behead("up-left", "coletor") %>% select(coletor) %>% unique ()

perguntas[,"pergunta_id"] %>% unique()[1]



perguntas %>%
  nest_by(pagina_id, pagina, pergunta_id, pergunta_num, pergunta, tipo, subtipo) %>%
  nest_by(pagina_id, pagina)

unique(perguntas[,"pergunta_id"])















# ---------------
dados_1 <- survey_df %>%
  select(collector_id, `151230059`)

# Pergunta 1 - pizza geral ----
dados_1 %>%
  reframe(count(., .by = `151230059`)) %>%
  ggplot(aes(x = "",
             y = n,
             fill = .by)) +
  geom_bar(stat = "identity",
           width = 1,
           color = "white") +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Você concorda com o termo de consentimento, e deseja continuar o questionário?") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        legend.title = element_blank()) + 
  geom_text(aes(x = "", y = n, label = paste0(n, " \n", as.integer((n/sum(n))*100), "%")),
            position = position_stack(vjust = 0.5)) 

# Pergunta 1 - barras por collector ----

dados_1











metadados$pages[3]
metadados$pages[[3]]
# Dá certo mas achei dificil de entender
metadados$pages %>%
  map_dfr(~{
    pagina_id <- .x$position
    pagina <- .x$title
    questions <- .x$questions %>%
      map_dfr(~{
        pergunta_id <- .x$id
        pergunta <- .x$headings[[1]]$heading %>% str_remove_all("<.*?>|</.*?>")
        tipo <- .x$family
        subtipo <- .x$subtype
        choices <- .x$answers$choices %>%
          map_df(~tibble(
            resposta_id = .x$id,
            resposta = .x$text %>% str_remove_all("<.*?>|</.*?>"),
            peso = .x$weight
          ))
        tibble(pagina_id, pagina, pergunta_id, pergunta, tipo, subtipo, choices)
      })
    questions
  })



i <- 1
j <- 1
k <- 1

pagina_id <- integer()
pagina_nome <- character()
pergunta_id <- character()
pergunta_nome <- character()
pergunta_tipo <- character()
pergunta_subtipo <- character()
resposta_opcao <- character()
resposta_peso <- character()

for (i in 1:length(metadados$pages)) {
  seq_perg <- 1:length(metadados$pages[[i]]$questions)
  
  for (j in seq_perg) {
    seq_resp <- 1:length(metadados$pages[[i]]$questions[[j]]$answers$choices)
    
    for (k in seq_resp) {
      pagina_id <- append(pagina_id, metadados$pages[[i]]$position)
      pagina_nome <- append(pagina_nome, metadados$pages[[i]]$title %>% str_remove_all("<.*?>|</.*?>"))
      
      pergunta_id <- append(pergunta_id, metadados$pages[[i]]$questions[[j]]$id)
      pergunta_nome <- append(pergunta_nome, metadados$pages[[i]]$questions[[j]]$headings[[1]]$heading %>% str_remove_all("<.*?>|</.*?>"))
      pergunta_tipo <- append(pergunta_tipo, metadados$pages[[i]]$questions[[j]]$family)
      pergunta_subtipo <- append(pergunta_subtipo, metadados$pages[[i]]$questions[[j]]$subtype)
      
      if(length(metadados$pages[[i]]$questions[[j]]$answers$choices[[k]]$text) == 0) {
        resposta_opcao <- append(resposta_opcao, NA)
      } else {
        resposta_opcao <- append(resposta_opcao, metadados$pages[[i]]$questions[[j]]$answers$choices[[k]]$text %>% str_remove_all("<.*?>|</.*?>"))
      }
      
      if(length(metadados$pages[[i]]$questions[[j]]$answers$choices[[k]]$weight) == 0) {
        resposta_peso <- append(resposta_peso, NA)
      } else {
        resposta_peso <- append(resposta_peso, metadados$pages[[i]]$questions[[j]]$answers$choices[[k]]$weight)
      }
    }
  }
}  

tabela_perguntas <- 
  tibble(pagina_id,
         pagina_nome,
         pergunta_id,
         pergunta_nome,
         pergunta_tipo,
         pergunta_subtipo,
         resposta_opcao,
         resposta_peso)

tabela_perguntas %>% 

tabela_perguntas %>% 
  mutate(publico = case_match(pagina_nome,
                              str_detect(pagina_nome, "^TC") ~ "TC"))
  mutate(tipo = case_match(pergunta_tipo,
                           "presentation" ~ "Abertura")) %>%
  View()
