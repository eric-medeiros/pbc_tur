install.packages("devtools")
devtools::install_github("soubhikbarari/svmkR")
library(svmkR)
library(wrapr)
library(dplyr)
library(forcats)
library(ggplot2)

usethis::edit_r_profile()

# no arquivo que abrir:
# options(sm_oauth_token = "<colar token>")

# verificar
getOption("sm_oauth_token")


surveys <- browse_surveys(10)
View(surveys)

metadados <- fetch_survey_obj(514390303)

perguntas <- tibble(
  codigo = as.numeric(names(metadados$questions)),
  pergunta = unlist(unname(metadados$questions))
)

# View(a)


# Respostas - your survey's ID goes here
survey_df <- fetch_survey_obj(514390303) %>% parse_survey()
View(survey_df)

dados <- survey_df %>%
  select(-survey_id, -collection_mode, -first_name, -last_name, -email_address) %>%
  select(`151230059`) %>%
  reframe(count(., .by = `151230059`))


ggplot(dados, aes(x = "", y = n, fill = .by)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Você concorda com o termo de consentimento, e deseja continuar o questionário?") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = dados,
            aes(x = "", y = n, label = n),
            position = position_stack(vjust = 0.5)) 
