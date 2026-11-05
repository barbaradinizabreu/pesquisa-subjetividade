suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("dados_classificados_pnadc_topic4_01.csv")) {
  stop("Arquivo intermediário não encontrado: dados_classificados_pnadc_topic4_01.csv")
}

dados <- read.csv("dados_classificados_pnadc_topic4_01.csv", stringsAsFactors = FALSE)
saveRDS(dados, "dados_classificados_pnadc_topic4.rds")
message("Arquivo RDS salvo: dados_classificados_pnadc_topic4.rds")
