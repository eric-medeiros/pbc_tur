vl_graphs <- function(dados_stats) {
  source("01_scripts/vl_gr_violin_c_hosp.R")
  source("01_scripts/vl_gr_violin_c_transp.R")
  source("01_scripts/vl_gr_violin_perm.R")
  source("01_scripts/vl_gr_violin_previsao.R")
  source("01_scripts/vl_gr_violin_passeios_boto.R")
  source("01_scripts/vl_gr_violin_passeios_geral.R")
  source("01_scripts/vl_gr_reg_prev_calc.R")
  source("01_scripts/vl_gr_violin_c_total.R")
  source("01_scripts/vl_gr_ec_cm.R")
  source("01_scripts/vl_gr_ec_sm.R")
  
  # COMEÇAR A INVESTIGAR ORIGENS (INVESTIGAR DIFERENÇA - FISHER, G, OU QUI²)
  
  g1 <- vl_gr_violin_c_hosp(dados_stats)
  g2 <- vl_gr_violin_c_transp(dados_stats)
  g3 <- vl_gr_violin_perm(dados_stats)
  g4 <- vl_gr_violin_previsao(dados_stats)
  g5 <- vl_gr_violin_passeios_boto(dados_stats)
  g6 <- vl_gr_violin_passeios_geral(dados_stats)
  g7 <- vl_gr_reg_prev_calc(dados_stats)
  g8 <- vl_gr_violin_c_total(dados_stats)  
  g9 <- vl_gr_ec_sm(dados_stats)
  g10 <- vl_gr_ec_cm(dados_stats)
  
  result <- list(
    g1 = g1,
    g2 = g2,
    g3 = g3,
    g4 = g4,
    g5 = g5,
    g6 = g6,
    g7 = g7,
    g8 = g8,
    g9 = g9,
    g10 = g10
  )
  
  return(result)
}  
