library(RCurl)
library(dplyr)

source('http://cemin.wikidot.com/local--files/raisr/rais.r', encoding = "latin1")

# url <- "ftp://ftp.mtps.gov.br/pdet/microdados"
# filenames <- getURL(url), ftp.use.epsv = FALSE,dirlistonly = TRUE) 


load("results/SP1999.Rda")

cananeia <- 3509908

SP1999 %>%
  tibble() %>% 
  rename(
    municipio = Município,
    tipo1 = "Tipo Estab1",
    tipo2 = "Tipo Estab2") %>%
  filter(municipio == cananeia)
