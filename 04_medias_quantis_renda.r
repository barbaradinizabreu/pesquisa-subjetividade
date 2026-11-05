suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(writexl)
})

dados <- readRDS("dados_classificados_pnadc_topic4.rds")

base <- dados %>%
  mutate(VDI4046 = as.numeric(VDI4046)) %>%
  filter(!is.na(classe_ipea), !is.na(VDI4046), VDI4046 > 0, VDI4046 < 1e6)

design <- svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1033, data = base, nest = TRUE)

media <- svyby(~VDI4046, by = ~classe_ipea, design = design, FUN = svymean)
media <- media %>% rename(media_faixa_renda = VDI4046, erro_padrao = se)

lista_quantis <- lapply(unique(base$classe_ipea), function(classe) {
  desenho_classe <- subset(design, classe_ipea == classe)
  quantis_classe <- svyquantile(~VDI4046, design = desenho_classe, quantiles = c(0, 1), ci = FALSE)
  valores_quantis <- as.numeric(quantis_classe[[1]])
  data.frame(
    classe_ipea = classe,
    faixa_renda_minima = valores_quantis[1],
    faixa_renda_maxima = valores_quantis[length(valores_quantis)]
  )
})
lista_quantis <- bind_rows(lista_quantis)

tabela <- media %>% left_join(lista_quantis, by = "classe_ipea") %>% select(classe_ipea, media_faixa_renda, erro_padrao, faixa_renda_minima, faixa_renda_maxima)

write_xlsx(tabela, "tabela_resumo_renda_ocupacao.xlsx")
print(tabela)
message("Arquivo salvo: tabela_resumo_renda_ocupacao.xlsx")
