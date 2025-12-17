library(sf)
library(here)
library(dplyr)
library(units)

input_trajetos <- here::here("//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/03_ANEL DE LAMA/04_MAPAS/ESTOURO/Georefs/trajetos.gpkg")
output_trajetos <- here::here("03_results/trajetos_sel.gpkg")

trajeto <- 
  st_read(input_trajetos, quiet = TRUE) %>%
  filter(!st_is_empty(geom)) %>%
  mutate(traj_id = row_number()) %>%
  st_cast("POINT", warn = FALSE) %>%
  group_by(traj_id)

# thresholds
thr_min <- set_units(100, m)
thr_max <- set_units(1000, m)
step_dist <- set_units(500, m)

sel_geral <- list()

for (i in unique(trajeto$traj_id)) {
  
  metadados_traj <- trajeto %>% filter(traj_id == i) %>% st_drop_geometry() %>% head(1)
  pontos_traj <- trajeto %>% filter(traj_id == i) %>% st_geometry()
  sel <- pontos_traj[1,]
  
  for (p in 2:length(pontos_traj)) {
    
    distancia <- st_distance(pontos_traj[p,], tail(sel, 1))[[1]]
    
    if (distancia < thr_min) { next }
    
    if (distancia >= thr_min & distancia < thr_max) { sel <- sel %>% append(pontos_traj[p,]) }
    
    if (distancia >= thr_max) {
      
      pontos_criados <- 
        tail(sel, 1) %>%
        append(pontos_traj[p,]) %>%
        st_combine() %>%
        st_cast("LINESTRING") %>%
        st_segmentize(dfMaxLength = step_dist) %>%
        st_cast("POINT") %>%
        tail(.,-1)
      
      sel <- sel %>% append(pontos_criados)   
    }
  }
  sel_geral[[length(sel_geral) + 1]] <- st_sf(metadados_traj, geometry = sel %>% st_combine())
}

sel_geral %>% bind_rows() %>% st_write(output_trajetos, append = FALSE)

# joga no qgis e faz kernel com célula de 170m e raio de kernel de 500m
# depois poliganiza
# depois normaliza valores para ficarem entre 0 e 1
# depois dissolve 2 vezes com seleção dos top 95% e dos top 50%
