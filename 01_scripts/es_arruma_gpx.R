library(sf)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

pasta_geral <- here::here("//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/03_ANEL DE LAMA/04_MAPAS/ESTOURO/Georefs/01_rotas_selecao")
output_botos <- here::here("//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/03_ANEL DE LAMA/04_MAPAS/ESTOURO/Georefs/botos.gpkg")
output_trajetos <- here::here("//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/03_ANEL DE LAMA/04_MAPAS/ESTOURO/Georefs/trajetos.gpkg")


dados_nome <- 
  tibble(caminho_pasta = list.files(pasta_geral, full.names = TRUE),
         nome_pasta = basename(caminho_pasta)) %>%
  filter(nome_pasta != "RETIRADO") %>%
  mutate(caminho_arquivo = map(caminho_pasta, ~ list.files(.x, full.names = TRUE))) %>%
  unnest(cols = c(caminho_arquivo)) %>%
  mutate(nome_arquivo = basename(caminho_arquivo),
         id_user = str_sub(nome_pasta, 6, -1),
         id_sub = str_sub(nome_arquivo, 1, 3),
         tipo_gpx = case_when(
           str_detect(nome_arquivo, "trajeto") ~ "trajeto",
           str_detect(nome_arquivo, "boto") ~ "boto"),
         destino = str_sub(
           nome_arquivo,
           str_locate(nome_arquivo, "_user_[:digit:]+_")[,"end"] + 1,
           str_locate(nome_arquivo, "_[:alpha:]+.gpx")[,"start"] - 1
         ))

dados_botos <- 
  dados_nome %>%
  filter(tipo_gpx == "boto") %>%
  mutate(pontos = map(caminho_arquivo, ~ st_read(.x, layer = "track_points", quiet = TRUE))) %>%
  unnest(pontos) %>%
  select(id_user, destino, id_sub, id_ponto = track_seg_point_id, data_hora_ponto = time, tipo_ponto = sym, geometry) %>%
  st_as_sf()

dados_botos %>%
  st_write(output_botos)

dados_trajetos <- 
  dados_nome %>%
  filter(tipo_gpx == "trajeto") %>%
  mutate(track_points = map(caminho_arquivo, ~ st_read(.x, layer = "track_points", quiet = TRUE))) %>%
  unnest(track_points) %>%
  st_as_sf() %>%
  group_by(id_user, destino, id_sub) %>%
  summarise(
    data_inicio = min(time, na.rm = TRUE),
    data_fim = max(time, na.rm = TRUE),
    geometry = st_combine(geometry), .groups = "drop"
  ) %>%
  mutate(geometry = st_cast(geometry, "LINESTRING"))

dados_trajetos %>%
  st_write(output_trajetos)
