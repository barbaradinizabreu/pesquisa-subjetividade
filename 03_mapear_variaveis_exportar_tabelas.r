suppressPackageStartupMessages({
  library(dplyr)
  library(writexl)
})

mapa_variaveis <- c(
  S07001 = "usou_internet",
  S07001A = "frequencia_usa_internet",
  S07005 = "motivo_nao_uso_internet",
  S07005B1 = "uso_recente_email",
  S07005B2 = "uso_recente_outros",
  S07005B3 = "uso_recente_redes_sociais",
  S07005B4 = "uso_recente_streaming",
  S07005B5 = "uso_recente_aplicativos"
)

if (!file.exists("dados_classificados_pnadc_topic4.rds")) {
  stop("Arquivo RDS não encontrado: dados_classificados_pnadc_topic4.rds")
}

dados <- readRDS("dados_classificados_pnadc_topic4.rds")

exportar_tabelas <- function(df, variavel_grupo, prefixo_arquivo) {
  for (codigo in names(mapa_variaveis)) {
    nome_legivel <- mapa_variaveis[[codigo]]

    if (!codigo %in% names(df)) {
      message("Variável não encontrada: ", codigo)
      next
    }

    tabela <- df %>%
      filter(!is.na(.data[[variavel_grupo]]), !is.na(.data[[codigo]])) %>%
      group_by(.data[[variavel_grupo]], .data[[codigo]], .drop = TRUE) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[variavel_grupo]], .drop = TRUE) %>%
      mutate(proporcao = n / sum(n)) %>%
      ungroup()

    if (nrow(tabela) == 0) {
      message("Ignorada (sem observações): ", codigo)
      next
    }

    nome_arquivo <- paste0(prefixo_arquivo, nome_legivel, ".xlsx")
    writexl::write_xlsx(tabela, nome_arquivo)
    message("Exportado: ", nome_arquivo)
  }
}

exportar_tabelas(dados, "FaixaRenda", "tabela_renda_")
exportar_tabelas(dados, "classe_ipea", "tabela_ipea_")
exportar_tabelas(dados, "ClasseSocial", "tabela_classe_social_")
message("Exportação concluída.")
