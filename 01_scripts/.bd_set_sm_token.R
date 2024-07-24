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