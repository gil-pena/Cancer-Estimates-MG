##Scritp R para o preparo dos bancos de população
## Gil Pena

rm(list=ls(all=TRUE))
gc()
Drive<-"E:"
setwd(file.path(Drive, "Estimativas2024MacroCorretas/Populacao"))

if (!require(dplyr)) install.packages("dplyr");library(dplyr)
if (!require(tidyr)) install.packages("tidyr");library(tidyr)
if (!require(readxl)) install.packages("readxl");library(readxl)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

##### Organiza os bancos de populacao 2013 a 2021

## faz lista dos arquivos do diretorio, especifica o sexo descrito.
## arquivos obtidos do datasus, antes retira cabeçalho, rodapé e linha de título.
BancosPop2013_21<-as.data.frame(list.files(file.path(Drive, "Estimativas/Populacao/POP2013-2021"), full.names = TRUE))
colnames(BancosPop2013_21)<-c("Arquivo")
BancosPop2013_21$SEXO<-NA
BancosPop2013_21$SEXO[substr(BancosPop2013_21$Arquivo,43,45)=="Fem"]<-"2"
BancosPop2013_21$SEXO[substr(BancosPop2013_21$Arquivo,43,46)=="Masc"]<-"1"

### Faz um data.frame para todas as bases
Populacao<-data.frame()
for(i in 1:length(BancosPop2013_21$Arquivo)){
PopProv<-read.csv2(BancosPop2013_21$Arquivo[i], header = FALSE)
colnames(PopProv)<-c("Municipio", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                     "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", "FXETARIA7579", "FXETARIA8099")
PopProv$ANO<-substr(BancosPop2013_21$Arquivo[i], 39, 42)
PopProv$SEXO<-BancosPop2013_21$SEXO[i]
PopProv$MUNIC_RES<-substr(PopProv$Municipio, 1, 6)
Populacao<-rbind(Populacao, data.frame(PopProv$ANO,
                             PopProv$MUNIC_RES,
                             PopProv$SEXO,
                             PopProv$FXETARIA0004, 
                             PopProv$FXETARIA0509,
                             PopProv$FXETARIA1014, 
                             PopProv$FXETARIA1519, 
                             PopProv$FXETARIA2024, 
                             PopProv$FXETARIA2529, 
                             PopProv$FXETARIA3034, 
                             PopProv$FXETARIA3539, 
                             PopProv$FXETARIA4044, 
                             PopProv$FXETARIA4549, 
                             PopProv$FXETARIA5054, 
                             PopProv$FXETARIA5559, 
                             PopProv$FXETARIA6064, 
                             PopProv$FXETARIA6569, 
                             PopProv$FXETARIA7074, 
                             PopProv$FXETARIA7579, 
                             PopProv$FXETARIA8099)
)
rm(PopProv)
}
colnames(Populacao)<- c("ANO", "MUNIC_RES", "SEXO", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                     "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", "FXETARIA7579", "FXETARIA8099")

## Planilha geral dos municípios e macros obtidas de 
## https://www.saude.mg.gov.br/gestor/regionalizacao [acesso em 29/01/2024]estava com erro no código do IBGE!!!
## https://www.saude.mg.gov.br/images/1_noticias/06_2023/2-jul-ago-set/regionalizacao/3-Regionaliza%C3%A7%C3%A3o_2023.rar
### Usando planilha da Ritinha (Sinan)
Municipios<-read.csv2(file.path(Drive, "TabelasAux/TABELA DE MUNICÍPIOS PDR 2024-Rita-SINAN.csv"),
                      fileEncoding = "Latin1")


Municipios<-Municipios |>
             select(CÓDIGO, NM_MUNICIP, REGIÃO.AMPLIADA.DE.SAÚDE)

colnames(Municipios)<-c("CODMUNRES", "Municipio", "MacroRegiao")

#### faz a uniao "join" dos municipios com a base Pop, de acordo com a PROCEDEN
Populacao$MUNIC_RES<-as.integer(Populacao$MUNIC_RES)
POPMG2013_2021<-left_join(Populacao, Municipios, join_by("MUNIC_RES" == "CODMUNRES"))

##### muda o formato do arquivo de largo (colunas = faixas etárias) para longo (uma coluna para os totais)


POPMG2013_2021L<-pivot_longer(
           POPMG2013_2021, 
           cols = starts_with("FXETARIA"),
           names_to = "FXETARIA",
           names_prefix = "FXETARIA",
           values_to = "POPULACAO")

POPMG2013_2021L$POPULACAO<-as.integer(POPMG2013_2021L$POPULACAO)

###### Agrupa pela MacroRegiao e soma as populações por macro
POPMG2013_2021Final<-POPMG2013_2021L%>%select(MacroRegiao,ANO,SEXO,FXETARIA,POPULACAO)%>%
                      group_by(MacroRegiao,ANO,SEXO,FXETARIA)%>%
                      summarize(POPMACRO = sum(POPULACAO))

##########################
##########################

### Organiza os bancos das populações de 1980 a 2012 (baixados do TABWIN - DATA sus)
### Já vem no formato "longo" e com populações de todos estados

BancosPop1980_2012<-as.data.frame(list.files(file.path(Drive,"Estimativas/Populacao/POP1980-2012"), full.names = TRUE))
colnames(BancosPop1980_2012)<-c("Arquivo")

Populacao1<-data.frame()
for(i in 1:length(BancosPop1980_2012$Arquivo)){
PopProv1<-read.csv(BancosPop1980_2012$Arquivo[i])
Populacao1<-rbind(Populacao1, data.frame(PopProv1$ANO[substr(PopProv1$MUNIC_RES, 1, 2) == "31"],
                                         PopProv1$MUNIC_RES[substr(PopProv1$MUNIC_RES, 1, 2) == "31"],
                                         PopProv1$SEXO[substr(PopProv1$MUNIC_RES, 1, 2) == "31"],
                                         PopProv1$FXETARIA[substr(PopProv1$MUNIC_RES, 1, 2) == "31"], 
                                         PopProv1$POPULACAO[substr(PopProv1$MUNIC_RES, 1, 2) == "31"])
)
rm(PopProv1)
}
colnames(Populacao1)<- c("ANO", "MUNIC_RES", "SEXO", "FXETARIA5", "POPULACAO")

### Cria colunas para as grupos de faixas etarias 
Populacao1$FXETARIA5[substr(Populacao1$FXETARIA5,1,1) == "I"]<-"9999" #### IDADE IGNORADA
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)>=0 & as.integer(Populacao1$FXETARIA5)<=404]<-"0004"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)>=505 & as.integer(Populacao1$FXETARIA5)<=909]<-"0509"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)>=1010 & as.integer(Populacao1$FXETARIA5)<=1414]<-"1014"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)>=1515 & as.integer(Populacao1$FXETARIA5)<=1919]<-"1519"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==2024]<-"2024"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==2529]<-"2529"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==3034]<-"3034"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==3539]<-"3539"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==4044]<-"4044"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==4549]<-"4549"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==5054]<-"5054"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==5559]<-"5559"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==6064]<-"6064"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==6569]<-"6569"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==7074]<-"7074"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==7579]<-"7579"
Populacao1$FXETARIA[as.integer(Populacao1$FXETARIA5)==8099]<-"8099"

#### faz a uniao "join" dos municipios com a base Pop, de acordo com a CODMUNRES

POPMG1980_2012<-left_join(Populacao1, Municipios, join_by("MUNIC_RES" == "CODMUNRES"))

### agrupa as populações por macroregiao
POPMG1980_2012Final<-POPMG1980_2012%>%select(MacroRegiao,ANO,SEXO,FXETARIA,POPULACAO)%>%
                      group_by(MacroRegiao,ANO,SEXO,FXETARIA)%>%
                      summarize(POPMACRO = sum(POPULACAO))

#### prepara a população de 2022 (censo)
############################################# parte 1 masculino

POPMG2022Masc<-read_xlsx(path = file.path(Drive, "Estimativas2024/Populacao/Censo2022/tabela9514homens.xlsx"), 
                          skip = 1, col_names = FALSE)
colnames(POPMG2022Masc)<-c("CODMUNRES", "Municipio", "Total", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", 
              "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", "FXETARIA3539", "FXETARIA4044", 
              "FXETARIA4549", "FXETARIA5054", "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
              "FXETARIA7579", "FXETARIA8084", "FXETARIA8589", "FXETARIA9094", "FXETARIA9599", "FXETARIA100")

POPMG2022Masc<-POPMG2022Masc[substr(POPMG2022Masc$CODMUNRES, 1,2) == "31",]

POPMG2022Masc$FXETARIA8589<-as.numeric(POPMG2022Masc$FXETARIA8589)
POPMG2022Masc$FXETARIA9094<-as.numeric(POPMG2022Masc$FXETARIA9094)
POPMG2022Masc$FXETARIA9599<-as.numeric(POPMG2022Masc$FXETARIA9599)
POPMG2022Masc$FXETARIA100<-as.numeric(POPMG2022Masc$FXETARIA100)

POPMG2022Masc$FXETARIA8589[is.na(POPMG2022Masc$FXETARIA8589)]<-0
POPMG2022Masc$FXETARIA9094[is.na(POPMG2022Masc$FXETARIA9094)]<-0
POPMG2022Masc$FXETARIA9599[is.na(POPMG2022Masc$FXETARIA9599)]<-0
POPMG2022Masc$FXETARIA100[is.na(POPMG2022Masc$FXETARIA100)]<-0

#### cria coluna idade 80 a 99+
POPMG2022Masc$FXETARIA8099 <- POPMG2022Masc$FXETARIA8084 +  
                              POPMG2022Masc$FXETARIA8589 +
                              POPMG2022Masc$FXETARIA9094 +
                              POPMG2022Masc$FXETARIA9599 +
                              POPMG2022Masc$FXETARIA100

#Exclui colunas de idade 80-84,, 85-89, 90-94, 95-99, 100+ e coluna de total
POPMG2022Masc$Total<-NULL
POPMG2022Masc$FXETARIA8084<-NULL
POPMG2022Masc$FXETARIA8589<-NULL
POPMG2022Masc$FXETARIA9094<-NULL
POPMG2022Masc$FXETARIA9599<-NULL
POPMG2022Masc$FXETARIA100<-NULL
POPMG2022Masc$Municipio<-NULL
POPMG2022Masc$SEXO<-1
POPMG2022Masc$ANO<-2022

POPMG2022Masc$CODMUNRES<-as.integer(substr(POPMG2022Masc$CODMUNRES,1,6))
POPMG2022Masc<-left_join(POPMG2022Masc, Municipios)

POPMG2022MascL<-pivot_longer(
           POPMG2022Masc, 
           cols = starts_with("FXETARIA"),
           names_to = "FXETARIA",
           names_prefix = "FXETARIA",
           values_to = "POPULACAO")

######################### 2022 feminino

POPMG2022Fem<-read_xlsx(path = file.path(Drive, "Estimativas2024/Populacao/Censo2022/tabela9514Mulheres.xlsx"), 
                          skip = 1, col_names = FALSE)
colnames(POPMG2022Fem)<-c("CODMUNRES", "Municipio", "Total", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", 
              "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", "FXETARIA3539", "FXETARIA4044", 
              "FXETARIA4549", "FXETARIA5054", "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
              "FXETARIA7579", "FXETARIA8084", "FXETARIA8589", "FXETARIA9094", "FXETARIA9599", "FXETARIA100")

POPMG2022Fem<-POPMG2022Fem[substr(POPMG2022Fem$CODMUNRES, 1,2) == "31",]

POPMG2022Fem$FXETARIA8589<-as.numeric(POPMG2022Fem$FXETARIA8589)
POPMG2022Fem$FXETARIA9094<-as.numeric(POPMG2022Fem$FXETARIA9094)
POPMG2022Fem$FXETARIA9599<-as.numeric(POPMG2022Fem$FXETARIA9599)
POPMG2022Fem$FXETARIA100<-as.numeric(POPMG2022Fem$FXETARIA100)

POPMG2022Fem$FXETARIA8589[is.na(POPMG2022Fem$FXETARIA8589)]<-0
POPMG2022Fem$FXETARIA9094[is.na(POPMG2022Fem$FXETARIA9094)]<-0
POPMG2022Fem$FXETARIA9599[is.na(POPMG2022Fem$FXETARIA9599)]<-0
POPMG2022Fem$FXETARIA100[is.na(POPMG2022Fem$FXETARIA100)]<-0

#### cria coluna idade 80 a 99+
POPMG2022Fem$FXETARIA8099 <- POPMG2022Fem$FXETARIA8084 +  
                              POPMG2022Fem$FXETARIA8589 +
                              POPMG2022Fem$FXETARIA9094 +
                              POPMG2022Fem$FXETARIA9599 +
                              POPMG2022Fem$FXETARIA100

#Exclui colunas de idade 80-84,, 85-89, 90-94, 95-99, 100+ e coluna de total
POPMG2022Fem$Total<-NULL
POPMG2022Fem$FXETARIA8084<-NULL
POPMG2022Fem$FXETARIA8589<-NULL
POPMG2022Fem$FXETARIA9094<-NULL
POPMG2022Fem$FXETARIA9599<-NULL
POPMG2022Fem$FXETARIA100<-NULL
POPMG2022Fem$Municipio<-NULL
POPMG2022Fem$SEXO<-2
POPMG2022Fem$ANO<-2022

POPMG2022Fem$CODMUNRES<-as.integer(substr(POPMG2022Fem$CODMUNRES,1,6))
POPMG2022Fem<-left_join(POPMG2022Fem, Municipios)

POPMG2022FemL<-pivot_longer(
           POPMG2022Fem, 
           cols = starts_with("FXETARIA"),
           names_to = "FXETARIA",
           names_prefix = "FXETARIA",
           values_to = "POPULACAO")

POPMG2022<-rbind(POPMG2022MascL, POPMG2022FemL)

###### Agrupa pela MacroRegiao e soma as populações por macro
POPMG2022Final<-POPMG2022 %>% select(MacroRegiao,ANO,SEXO,FXETARIA,POPULACAO)%>%
                      group_by(MacroRegiao,ANO,SEXO,FXETARIA)%>%
                      summarize(POPMACRO = sum(POPULACAO))


#### Reune as bases de população, cria ano 1979 = 1980.

POPMG1980_2012Final$ANO <- as.character(POPMG1980_2012Final$ANO)
POPMG1980_2012Final$SEXO <- as.character(POPMG1980_2012Final$SEXO)
Pop1979<-POPMG1980_2012Final[POPMG1980_2012Final$ANO == "1980",]
Pop1979$ANO <- "1979"
POPMG2022Final$ANO <- as.character(POPMG2022Final$ANO)
POPMG2022Final$SEXO <- as.character(POPMG2022Final$SEXO)

PopMacroMG<-rbind(Pop1979, POPMG1980_2012Final, POPMG2013_2021Final, POPMG2022Final)

PopMacroMG1<-PopMacroMG[PopMacroMG$FXETARIA != "9999",] ##### remove faixas etÃ¡rias ignoradas

write.csv2(PopMacroMG1, file = "PopulacaoMacrosMG1979-2022.csv", row.names = FALSE, fileEncoding = "Latin1")

#######################################
#25/01/2024 - Populações preparadas
#######################################

############################# Piramides populacionais

###############exporta pirâmides das populações para arquivo 
ANOS<-levels(as.factor(PopMacroMG1$ANO))
pdf("Populacoes.pdf", width = 7, height = 10)
i=1
for(i in i:length(ANOS)){
print(ggplot(data = PopMacroMG1[PopMacroMG1$ANO == ANOS[i],],
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPMACRO, no = POPMACRO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       facet_wrap(~MacroRegiao)+
       theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
       ggtitle(ANOS[i]))
}
dev.off()
rm(i)
################# pirâmides 2022
MacroRegiao<-levels(as.factor(PopMacroMG1$MacroRegiao))
pdf("Populacoes2022.pdf", width = 7, height = 10)
i=1
for(i in i:length(MacroRegiao)){
print(ggplot(data = PopMacroMG1[PopMacroMG1$ANO == 2022,],
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPMACRO, no = POPMACRO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       ggtitle(MacroRegiao[i]))
}
dev.off()
rm(i)

################### Prepara a tabela de populacao Belo horizonte e poços de caldas
##### Belo Horizonte

POPBH2013_2021L<-POPMG2013_2021L[POPMG2013_2021L$Municipio == "Belo Horizonte",]
POPBH2013_2021Final<-POPBH2013_2021L%>%select(ANO,SEXO,FXETARIA,POPULACAO)


POPBH1980_2012<-POPMG1980_2012[POPMG1980_2012$Municipio == "Belo Horizonte",]
### não está agrupado pela faixa etaria de 5 em 5
POPBH1980_2012Final<-POPBH1980_2012%>%select(ANO,SEXO,FXETARIA,POPULACAO)%>%
                      group_by(ANO,SEXO,FXETARIA)%>%
                      summarize(POPULACAO = sum(POPULACAO))
POPBH1980_2012Final$ANO<-as.character(POPBH1980_2012Final$ANO)
POPBH1980_2012Final$SEXO<-as.character(POPBH1980_2012Final$SEXO)
PopBH1979<-POPBH1980_2012Final[POPBH1980_2012Final$ANO == "1980",]
PopBH1979$ANO <- "1979"

PopBH2022<-POPMG2022[POPMG2022$Municipio == "Belo Horizonte",]
PopBH2022$ANO<-as.character(PopBH2022$ANO)
PopBH2022$SEXO<-as.character(PopBH2022$SEXO)

PopBH<-rbind(PopBH1979, POPBH1980_2012Final, POPBH2013_2021Final, PopBH2022)

PopBH1<-PopBH[PopBH$FXETARIA != "9999",] ##### remove faixas etÃ¡rias ignoradas

write.csv2(PopBH1, file = "PopulacaoBH1979-2022.csv", row.names = FALSE, fileEncoding = "Latin1")

##### 
pdf("PopulacoesBH.pdf", width = 7, height = 10)
print(
ggplot(data = PopBH1,
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       facet_wrap(~ANO)+
       theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
       ggtitle("Belo Horizonte")
)
dev.off()


##### pirâmides etarias belo horizonte
pdf("PopulacoesBH2022.pdf", width = 7, height = 7)
print(
ggplot(data = PopBH1[PopBH1$ANO=="2022",],
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       ggtitle("Belo Horizonte 2022")
)
dev.off()


##### Poços de Caldas

POPPC2013_2021L<-POPMG2013_2021L[POPMG2013_2021L$Municipio == "Poços de Caldas",]
POPPC2013_2021Final<-POPPC2013_2021L%>%select(ANO,SEXO,FXETARIA,POPULACAO)


POPPC1980_2012<-POPMG1980_2012[POPMG1980_2012$Municipio == "Poços de Caldas",]
### não está agrupado pela faixa etaria de 5 em 5
POPPC1980_2012Final<-POPPC1980_2012%>%select(ANO,SEXO,FXETARIA,POPULACAO)%>%
                      group_by(ANO,SEXO,FXETARIA)%>%
                      summarize(POPULACAO = sum(POPULACAO))
POPPC1980_2012Final$ANO<-as.character(POPPC1980_2012Final$ANO)
POPPC1980_2012Final$SEXO<-as.character(POPPC1980_2012Final$SEXO)
PopPC1979<-POPPC1980_2012Final[POPPC1980_2012Final$ANO == "1980",]
PopPC1979$ANO <- "1979"

PopPC2022<-POPMG2022[POPMG2022$Municipio == "Poços de Caldas",]
PopPC2022$ANO<-as.character(PopPC2022$ANO)
PopPC2022$SEXO<-as.character(PopPC2022$SEXO)

PopPC<-rbind(PopPC1979, POPPC1980_2012Final, POPPC2013_2021Final, PopPC2022)


PopPC1<-PopPC[PopPC$FXETARIA != "9999",] ##### remove faixas etÃ¡rias ignoradas

write.csv2(PopPC1, file = "PopulacaoPC1979-2022.csv", row.names = FALSE, fileEncoding = "Latin1")



##### pirâmides etarias Poços de Caldas
pdf("PopulacoesPC.pdf", width = 7, height = 10)
print(
ggplot(data = PopPC1,
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       facet_wrap(~ANO)+
       theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
       ggtitle("Poços de Caldas")
)
dev.off()
####################2022
pdf("PopulacoesPC2022.pdf", width = 7, height = 10)
print(
ggplot(data = PopPC1[PopPC1$ANO == "2022",],
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       ggtitle("Poços de Caldas")
)
dev.off()

################## Prepara a tabela de populacao para o estado

POPMGTOTAL<-PopMacroMG1%>%select(ANO,SEXO,FXETARIA,POPMACRO)%>%
                      group_by(ANO,SEXO,FXETARIA)%>%
                      summarize(POPULACAO = sum(POPMACRO))

#### grafico piramide da população
pdf("PopulacoesMG.pdf", width = 7, height = 10)
print(
ggplot(data = POPMGTOTAL,
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       facet_wrap(~ANO)+
       theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
       ggtitle("Minas Gerais")
)
dev.off()
##############
pdf("PopulacoesMG2022.pdf", width = 7, height = 10)
print(
ggplot(data = POPMGTOTAL[POPMGTOTAL$ANO == "2022",],
       mapping = aes(x = ifelse(test = SEXO == "1", yes = -POPULACAO, no = POPULACAO), 
                     y = FXETARIA, fill = SEXO)) + 
       geom_col() +
       labs(x = "População")+
       ggtitle("Minas Gerais - 2022")
)
dev.off()


write.csv2(POPMGTOTAL, file = "POPTOTALMG1979_2022.csv", row.names = FALSE)

PopBH1$MacroRegiao <-"Belo Horizonte"
PopBH1<-PopBH1 |>
        mutate(POPMACRO = POPULACAO)|>    
        select(MacroRegiao, ANO, SEXO, FXETARIA, POPMACRO)
        
PopPC1$MacroRegiao <-"Poços de Caldas"
PopPC1<-PopPC1 |>
        mutate(POPMACRO = POPULACAO)|>    
        select(MacroRegiao, ANO, SEXO, FXETARIA, POPMACRO)

POPMGTOTAL$MacroRegiao <-"Minas Gerais"
POPMGTOTAL<-POPMGTOTAL |>
        mutate(POPMACRO = POPULACAO)|>    
        select(MacroRegiao, ANO, SEXO, FXETARIA, POPMACRO)
  
PopMacroMG<-rbind(PopMacroMG, PopBH1, PopPC1, POPMGTOTAL)

write.csv2(PopMacroMG, file = "BancoFinalPop1979-2022MGporMacro-incluiBH-PC-MG.csv", row.names = FALSE, fileEncoding = "Latin1")

############################################
