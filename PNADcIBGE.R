# Baixando e carregando pacotes necessários para extração e análise dos microdados
install.packages ("PNADcIBGE")
library (PNADcIBGE)
install.packages(“survey”, “srvyr”)
library(survey, srvyr)

#Extraindo microdados
pnadc_anual_trimestre <- get_pnadc(year = 2025, topic = 4, defyear = 2025, defperiod = 4, labels = TRUE, deflator = TRUE, design = FALSE)

# Realizando coleta de lixo acumulada durante a obtenção dos microdados
gc(verbose=FALSE, reset=FALSE, full=TRUE)
 
# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, Pais=as.factor("Brasil"))
pnadc_anual_trimestre$Pais <- factor(x=pnadc_anual_trimestre$Pais, levels=c("Brasil"))
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_trimestre$GR <- factor(x=pnadc_anual_trimestre$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Realizando processo de obtenção da estimativa do rendimento domiciliar deflacionado - variáveis VDI5007real e VDI5008real
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, VD4019real=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*Habitual))
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, VDI4048real=ifelse(is.na(VDI4048) | is.na(V2001_rendimento) | VDI4048==99999999,NA,VDI4048*Efetivo))
pnadc_anual_trimestre_rendimento <- pnadc_anual_trimestre %>% dplyr::group_by(ID_DOMICILIO) %>% dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),                                                                                                      	rendimento_todos_trabalhos=sum(VD4019real, na.rm=TRUE),                                                                                                    	rendimento_outras_fontes=sum(VDI4048real, na.rm=TRUE))
pnadc_anual_trimestre_rendimento <- transform(pnadc_anual_trimestre_rendimento, VDI5007real=rendimento_todos_trabalhos+rendimento_outras_fontes)
pnadc_anual_trimestre_rendimento <- transform(pnadc_anual_trimestre_rendimento, VDI5008real=VDI5007real/moradores_rendimento)
pnadc_anual_trimestre <- pnadc_anual_trimestre[,!(names(pnadc_anual_trimestre) %in% c("V2001_rendimento","VD4019real","VDI4048real"))]
pnadc_anual_trimestre_rendimento <- pnadc_anual_trimestre_rendimento[,!(names(pnadc_anual_trimestre_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos","rendimento_outras_fontes"))]
pnadc_anual_trimestre <- merge(x=pnadc_anual_trimestre, y=pnadc_anual_trimestre_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_trimestre_rendimento)
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, VDI5007real=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)" | VDI5007==99999999,NA,VDI5007real))
pnadc_anual_trimestre <- transform(pnadc_anual_trimestre, VDI5008real=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)" | VDI5008==99999999,NA,VDI5008real))

#Criando variável de Classe Social
pnadc_anual_trimestre <- transform(
  pnadc_anual_trimestre,
  ClasseSocial = as.factor(
    x = ifelse(
      VDI5008real >= 0 & VDI5008real <= 1727,
      "Classe E",
      ifelse(
        VDI5008real > 1727 & VDI5008real <= 2760,
        "Classe D",
        ifelse(
          VDI5008real > 2760 & VDI5008real <= 11897,
          "Classe C",
          ifelse(
            VDI5008real > 11897 & VDI5008real <= 15510,
            "Classe B",
            ifelse(
              VDI5008real > 15510,
              "Classe A",
              NA)))))))

# Realizando processo de incorporação do desenho amostral nos microdados
pnadc_anual_trimestre <- tibble::as_tibble(x=pnadc_anual_trimestre)
pnadc_anual_trimestre <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_trimestre)
str(object=pnadc_anual_trimestre)


#Obtendo estimativas no nível domícilio
pnadc_domicilios <- subset(
pnadc_anual_trimestre,
V2005 == "Pessoa responsável pelo domicílio"
)

# Convertendo os dados para desenho amostral com srvyr
pnadc_anual_trimestre <- as_survey(pnadc_anual_trimestre)
pnadc_domicilios<- as_survey(pnadc_domicilios)
