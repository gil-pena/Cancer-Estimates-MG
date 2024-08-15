##Scritp R para o preparo dos bancos de mortalidade taxas
## Gil Pena

rm(list=ls(all=TRUE))
gc()
Drive<-"E:"
setwd(file.path(Drive, "Estimativas2024MacroCorretas/Mortalidade"))

if (!require(dplyr)) install.packages("dplyr");library(dplyr)
if (!require(tidyr)) install.packages("tidyr");library(tidyr)
if (!require(readxl)) install.packages("readxl");library(readxl)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(read.dbc)) install.packages("read.dbc", lib = "E:/R-4.3.2/library"); library(read.dbc)

### Organiza as bases 1996-2021
##Bancos do estado de MG baixados do Tabwin
Bancos<-as.data.frame(list.files(file.path(Drive, "Estimativas2024/SIM/BasesSIM1996-2022"), full.names = TRUE))
colnames(Bancos)<-c("Arquivo")
Base<-data.frame()
for(i in 1:length(Bancos$Arquivo)){
BaseProv<-read.dbc(Bancos$Arquivo[i])
BaseProv$ANO<-substr(Bancos$Arquivo[i], 46, 49)
Base<-rbind(Base, data.frame(BaseProv$ANO,
                             BaseProv$DTOBITO,
                             BaseProv$IDADE,
                             BaseProv$DTNASC,
                             BaseProv$SEXO,
                             BaseProv$CODMUNRES,
                             BaseProv$CAUSABAS))
rm(BaseProv)
}
rm(i)
colnames(Base)<- c("ANO", "DTOBITO", "IDADE", "DTNASC", "SEXO", "CODMUNRES", "CAUSABAS")

### Organiza as bases 1979-1995


Bancos1<-as.data.frame(list.files(file.path(Drive, "Estimativas2024/SIM/BasesSIM1979-1995"), full.names = TRUE))
colnames(Bancos1)<-c("Arquivo")

Base1<-data.frame()
for(i in 1:length(Bancos1$Arquivo)){
BaseProv<-read.dbc(Bancos1$Arquivo[i])
BaseProv$ANO<-paste0("19",substr(Bancos1$Arquivo[i], 47, 48))
Base1<-rbind(Base1, data.frame(BaseProv$ANO,
                             BaseProv$DATAOBITO,
                             BaseProv$IDADE,
                             BaseProv$DATANASC,
                             BaseProv$SEXO,
                             BaseProv$MUNIRES,
                             BaseProv$CAUSABAS))
rm(BaseProv)
}
colnames(Base1)<- c("ANO", "DTOBITO", "IDADE", "DTNASC", "SEXO", "CODMUNRES", "CAUSABAS")

Base$NEOPLASIA<-NA
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10")] <-"01-Cavidade Oral (C00-C10)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C15")] <-"02-Esofago (C15)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C16")] <-"03-Estomago(C16)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C18", "C19", "C20", "C21")] <- "04-Colon e Reto (C18-C21)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C22")] <- "05-Figado e vias biliares intra-hepaticas (C22)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C25")] <- "06-Pancreas (C25)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C32")] <- "07-Laringe (C32)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C33", "C34")] <- "08-Traqueia, bronquio e pulmao (C33-C34)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C43")] <- "09-Melanoma maligno da pele (C43)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C44")] <- "10-Outras neoplasias malignas da pele (C44)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C50")& Base$SEXO == "2"] <- "11-Mama feminina (C50)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C53")] <- "12-Colo do Utero (C53)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C54")] <- "13-Corpo do Utero (C54)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C55")] <- "Utero SOE(C55)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C56")] <- "14-Ovario (C56)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C61")] <- "15-Prostata (C61)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C67")] <- "16-Bexiga (C67)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C70", "C71", "C72")] <- "17-Sistema nervoso central (C70-C72)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C73")] <- "18-Glandula Tireoide (C73)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C81")] <- "19-Linfoma de Hodgkin (C81)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C82", "C83", "C84", "C85", "C96")] <- "20-Linfoma nao Hodgkin (C82-C85; C96)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C91", "C92", "C93", "C94", "C95")] <- "21-Leucemias (C91-C95)"
Base$NEOPLASIA[substr(Base$CAUSABAS, 1, 3) %in% c("C11", "C12", "C13", "C14", "C17", "C23", "C24", "C26", "C27", "C28", 
                                                  "C29", "C30", "C31", "C35", "C36", "C37", "C38", "C39", "C40",
                                                  "C41", "C45", "C46", "C47", "C48", "C49", "C51", "C52", 
                                                  "C57", "C58", "C59", "C60", "C62", "C63", "C64", "C65", "C66",
                                                  "C68", "C69", "C74", "C75", "C76", "C77", "C78", "C79", "C80",
                                                  "C86", "C88", "C90", "C97", "D46")] <- "22-Outras neoplasias (C00-C97; D46)"

Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("140", "141", "142", "144", "145", "146")] <-"01-Cavidade Oral (C00-C10)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("150")] <-"02-Esofago (C15)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("151")] <-"03-Estomago(C16)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("153", "154")] <- "04-Colon e Reto (C18-C21)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("155")] <- "05-Figado e vias biliares intra-hepaticas (C22)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("157")] <- "06-Pancreas (C25)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("161")] <- "07-Laringe (C32)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("162")] <- "08-Traqueia, bronquio e pulmao (C33-C34)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("172")] <- "09-Melanoma maligno da pele (C43)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("173")] <- "10-Outras neoplasias malignas da pele (C44)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("174")] <- "11-Mama feminina (C50)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("180")] <- "12-Colo do Utero (C53)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("182")] <- "13-Corpo do Utero (C54)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("179")] <- "Utero SOE(C55)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("183")] <- "14-Ovario (C56)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("185")] <- "15-Prostata (C61)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("188")] <- "16-Bexiga (C67)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("191", "192")] <- "17-Sistema nervoso central (C70-C72)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("193")] <- "18-Glandula Tireoide (C73)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("201")] <- "19-Linfoma de Hodgkin (C81)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("200", "202")] <- "20-Linfoma nao Hodgkin (C82-C85; C96)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("204", "205", "206", "207")] <- "21-Leucemias (C91-C95)"
Base1$NEOPLASIA[substr(Base1$CAUSABAS, 1, 3) %in% c("147", "148", "149", "152", "156", "158", "159", "160", "163", "164",
                                                    "165", "170", "171", "175", "176", "177", "178", "181", "184", "186", "187", 
                                                    "189", "190", "194", "195", "196", "197", "198",
                                                    "203", "238")] <- "22-Outras neoplasias (C00-C97; D46)"

Municipios<-read.csv2(file.path(Drive, "TabelasAux/TABELA DE MUNICÍPIOS PDR 2024-Rita-SINAN.csv"),
                      fileEncoding = "Latin1")
str(Municipios)

Municipios<-Municipios |>
             select(CÓDIGO, NM_MUNICIP, REGIÃO.AMPLIADA.DE.SAÚDE)
colnames(Municipios)<-c("CODMUNRES", "Municipio", "MacroRegiao")
#### faz a uniao "join" dos municipios com a base Sim, de acordo com a CODMUNRES
Base$CODMUNRES <- as.character(Base$CODMUNRES)
Base$CODMUNRES <- substr(Base$CODMUNRES, 1, 6)
Base$CODMUNRES <- as.integer(Base$CODMUNRES)
Base<-left_join(Base, Municipios, by = c("CODMUNRES"))

Base1$CODMUNRES <- as.character(Base1$CODMUNRES)
Base1$CODMUNRES <- substr(Base1$CODMUNRES, 1, 6)
Base1$CODMUNRES <- as.integer(Base1$CODMUNRES)
Base1<-left_join(Base1, Municipios, by = c("CODMUNRES"))

Base$SEXO<-as.character(Base$SEXO)
BaseNeoplasia<-filter(Base, SEXO == "1" | SEXO == "2", !is.na(Base$NEOPLASIA))

Base1$SEXO<-as.character(Base1$SEXO)
BaseNeoplasia1<-filter(Base1, SEXO == "1" | SEXO == "2", !is.na(Base1$NEOPLASIA))

BaseFinal<-rbind(BaseNeoplasia, BaseNeoplasia1)

BaseFinal$IDADE<-as.character(BaseFinal$IDADE)
BaseFinal$IDADE<-as.integer(BaseFinal$IDADE)
BaseFinal$FXETARIA<-NA
BaseFinal$FXETARIA[BaseFinal$IDADE==0]<-"9999"
BaseFinal$FXETARIA[BaseFinal$IDADE>=1 & BaseFinal$IDADE<=404]<-"0004"
BaseFinal$FXETARIA[BaseFinal$IDADE>=405 & BaseFinal$IDADE<=409]<-"0509"
BaseFinal$FXETARIA[BaseFinal$IDADE>=410 & BaseFinal$IDADE<=414]<-"1014"
BaseFinal$FXETARIA[BaseFinal$IDADE>=415 & BaseFinal$IDADE<=419]<-"1519"
BaseFinal$FXETARIA[BaseFinal$IDADE>=420 & BaseFinal$IDADE<=424]<-"2024"
BaseFinal$FXETARIA[BaseFinal$IDADE>=425 & BaseFinal$IDADE<=429]<-"2529"
BaseFinal$FXETARIA[BaseFinal$IDADE>=430 & BaseFinal$IDADE<=434]<-"3034"
BaseFinal$FXETARIA[BaseFinal$IDADE>=435 & BaseFinal$IDADE<=439]<-"3539"
BaseFinal$FXETARIA[BaseFinal$IDADE>=440 & BaseFinal$IDADE<=444]<-"4044"
BaseFinal$FXETARIA[BaseFinal$IDADE>=445 & BaseFinal$IDADE<=449]<-"4549"
BaseFinal$FXETARIA[BaseFinal$IDADE>=450 & BaseFinal$IDADE<=454]<-"5054"
BaseFinal$FXETARIA[BaseFinal$IDADE>=455 & BaseFinal$IDADE<=459]<-"5559"
BaseFinal$FXETARIA[BaseFinal$IDADE>=460 & BaseFinal$IDADE<=464]<-"6064"
BaseFinal$FXETARIA[BaseFinal$IDADE>=465 & BaseFinal$IDADE<=469]<-"6569"
BaseFinal$FXETARIA[BaseFinal$IDADE>=470 & BaseFinal$IDADE<=474]<-"7074"
BaseFinal$FXETARIA[BaseFinal$IDADE>=475 & BaseFinal$IDADE<=479]<-"7579"
BaseFinal$FXETARIA[BaseFinal$IDADE>=480]<-"8099"

BaseFinal$FXETARIA<-as.character(BaseFinal$FXETARIA)


#ObitosNeoplasiaGrupo<-BaseFinal%>%select(MacroRegiao,ANO,SEXO,NEOPLASIA,FXETARIA)%>%
#                      group_by(MacroRegiao,ANO,SEXO,NEOPLASIA,FXETARIA)%>%
#                      count()

ObitosNeoplasia<-table(BaseFinal$MacroRegiao,BaseFinal$ANO,BaseFinal$SEXO,BaseFinal$NEOPLASIA,BaseFinal$FXETARIA)

Obitos<-as.data.frame(ObitosNeoplasia)
str(Obitos)
colnames(Obitos)<-c("MacroRegiao","ANO","SEXO","NEOPLASIA","FXETARIA","Count")

ObitosNeoplasiaBH<-table(BaseFinal$MacroRegiao[BaseFinal$Municipio == "Belo Horizonte"],
                   BaseFinal$ANO[BaseFinal$Municipio == "Belo Horizonte"],
                   BaseFinal$SEXO[BaseFinal$Municipio == "Belo Horizonte"],
                   BaseFinal$NEOPLASIA[BaseFinal$Municipio == "Belo Horizonte"],
                   BaseFinal$FXETARIA[BaseFinal$Municipio == "Belo Horizonte"])

ObitosBH<-as.data.frame(ObitosNeoplasiaBH)
colnames(ObitosBH)<-c("MacroRegiao","ANO","SEXO","NEOPLASIA","FXETARIA","Count")
ObitosBH$MacroRegiao<-"Belo Horizonte"

ObitosNeoplasiaPC<-table(BaseFinal$MacroRegiao[BaseFinal$Municipio == "Poços de Caldas"],
                   BaseFinal$ANO[BaseFinal$Municipio == "Poços de Caldas"],
                   BaseFinal$SEXO[BaseFinal$Municipio == "Poços de Caldas"],
                   BaseFinal$NEOPLASIA[BaseFinal$Municipio == "Poços de Caldas"],
                   BaseFinal$FXETARIA[BaseFinal$Municipio == "Poços de Caldas"])


ObitosPC<-as.data.frame(ObitosNeoplasiaPC)
colnames(ObitosPC)<-c("MacroRegiao","ANO","SEXO","NEOPLASIA","FXETARIA","Count")
ObitosPC$MacroRegiao<-"Poços de Caldas"

ObitosNeoplasiaMG<-table(BaseFinal$ANO,BaseFinal$SEXO,BaseFinal$NEOPLASIA,BaseFinal$FXETARIA)

ObitosMG<-as.data.frame(ObitosNeoplasiaMG)
colnames(ObitosMG)<-c("ANO","SEXO","NEOPLASIA","FXETARIA","Count")
ObitosMG$MacroRegiao<-"Minas Gerais"

#write.csv2(ObitosNeoplasia, file = "ObitosNeoplasia.csv", row.names = FALSE)
#write.csv2(PopMacroMG1, file = "PopulacaoMGmacros1.csv", row.names = FALSE)

Obitos<-rbind(Obitos, ObitosBH, ObitosPC, ObitosMG)

write.csv2(Obitos, file = "ObitosNeoplasia.csv", row.names = FALSE, fileEncoding = "Latin1")

##################################################
#####29/01/2024 - cheguei até aqui
##################################################

###### Realocação dos óbitos C55
Macros<-levels(as.factor(Obitos$MacroRegiao))
Anos<-levels(as.factor(Obitos$ANO))
Realocacao<-data.frame()

m=1
for(m in m:length(Macros)){
a=1
for(a in a:length(Anos))  {

C53<-sum(Obitos$Count[Obitos$MacroRegiao == Macros[m] &
                 Obitos$ANO == Anos[a] &  
                 Obitos$NEOPLASIA == "12-Colo do Utero (C53)"])

C54<-sum(Obitos$Count[Obitos$MacroRegiao == Macros[m] &
                 Obitos$ANO == Anos[a] &  
                 Obitos$NEOPLASIA == "13-Corpo do Utero (C54)"])

C55<-sum(Obitos$Count[Obitos$MacroRegiao == Macros[m] &
                 Obitos$ANO == Anos[a] &  
                 Obitos$NEOPLASIA == "Utero SOE(C55)"])
P53<-C53/(C53+C54)

Realocacao<-rbind(Realocacao, data.frame(
                              Macros[m],
                              Anos[a],
                              C53,
                              C54,
                              C55,
                              P53))

}
}
colnames(Realocacao)<- c("MacroRegiao", "ANO", "C53", "C54", "C55", "P53")


Realocacao$P53[is.na(Realocacao$P53)]<-0.5 #### coloca resultado de divisao 0/0 como 0,5.


head(Realocacao)
RealocacaoAno<-Realocacao%>%group_by(ANO)%>%
                          summarize(C53 = sum(C53), C54 = sum(C54), C55 = sum(C55))  

head(RealocacaoAno)
write.csv2(Realocacao, "C55C54C53.csv", row.names = FALSE)

PropC55<-sum(Realocacao$C55)/(sum(Realocacao$C53)+sum(Realocacao$C54)+sum(Realocacao$C55))
PropC55
#######[1] 0.355319
#######(proporção excessiva, de acordo com artigo das cabras e ovelhas

ObitosRealocados<-Obitos
Macros<-levels(as.factor(ObitosRealocados$MacroRegiao))
Anos<-levels(as.factor(ObitosRealocados$ANO))
ObitosRealocados$Adiciona<-0
ObitosRealocados$Idade<-ifelse(test = as.character(ObitosRealocados$FXETARIA) < "5054", "Menor50", "Maior50")

m=1
for(m in m:length(Macros)){
a=1
for(a in a:length(Anos))  {
######De 0 a 49 anos -> C53*=C53+C54+C55 (planilha do INCA)
###### estou em dúvida no procedimento, vou alocar apenas os C55

ObitosRealocados$Adiciona[ObitosRealocados$NEOPLASIA == "12-Colo do Utero (C53)" &
                         ObitosRealocados$MacroRegiao == Macros[m] &
                         ObitosRealocados$ANO == Anos[a]&
                         ObitosRealocados$Idade == "Menor50"]<- 
ObitosRealocados$Count[ObitosRealocados$NEOPLASIA == "Utero SOE(C55)" &
                         ObitosRealocados$MacroRegiao == Macros[m] &
                         ObitosRealocados$ANO == Anos[a] &
                         ObitosRealocados$Idade == "Menor50"]

##### Acima de 50 anos -> C53**= C53+(C55*pC53)

ObitosRealocados$Adiciona[ObitosRealocados$NEOPLASIA == "12-Colo do Utero (C53)" &
                         ObitosRealocados$MacroRegiao == Macros[m] &
                         ObitosRealocados$ANO == Anos[a]&
                         ObitosRealocados$Idade == "Maior50"] <- 
round((ObitosRealocados$Count[ObitosRealocados$NEOPLASIA == "Utero SOE(C55)" &
                                  ObitosRealocados$MacroRegiao == Macros[m] &
                                  ObitosRealocados$ANO == Anos[a] &
                                  ObitosRealocados$Idade == "Maior50"]*
          Realocacao$P53[Realocacao$MacroRegiao == Macros[m] &
                         Realocacao$ANO == Anos[a]]),0)

ObitosRealocados$Adiciona[ObitosRealocados$NEOPLASIA == "13-Corpo do Utero (C54)" &
                         ObitosRealocados$MacroRegiao == Macros[m] &
                         ObitosRealocados$ANO == Anos[a] &
                         ObitosRealocados$Idade == "Maior50"] <-
round((ObitosRealocados$Count[ObitosRealocados$NEOPLASIA == "Utero SOE(C55)" &
                                  ObitosRealocados$MacroRegiao == Macros[m] &
                                  ObitosRealocados$ANO == Anos[a]&
                                  ObitosRealocados$Idade == "Maior50"] *
          (1-Realocacao$P53[Realocacao$MacroRegiao == Macros[m] &
                         Realocacao$ANO == Anos[a]])),0)



}
}
write.csv2(ObitosRealocados, "RealocacaoC55.csv", row.names = FALSE)

ObitosRev<-Obitos ####### Preserva data frame antes da realocacao

ObitosRealocados$Count<-ObitosRealocados$Adiciona + ObitosRealocados$Count ##### Realoca os casos

ObitosRealocados$Adiciona<-NULL
head(ObitosRealocados)


Obitos<-ObitosRealocados|>
         filter(NEOPLASIA != "Utero SOE(C55)")



Macros<-levels(as.factor(Obitos$MacroRegiao))
Sexos<-levels(as.factor(Obitos$SEXO))
Anos<-levels(as.factor(Obitos$ANO))
Neoplasias<-levels(as.factor(Obitos$NEOPLASIA))

table(Obitos$MacroRegiao)

Obitos$Idade<-NULL

str(Obitos)

ObitosTodasNeoplasias<-Obitos|>
              group_by(MacroRegiao, SEXO, ANO, FXETARIA)|>
              summarize(Count = sum(Count))

ObitosTodasNeoplasias$NEOPLASIA<-"TODAS NEOPLASIAS"

Obitos<-rbind(Obitos, ObitosTodasNeoplasias)

#Macros<-Macros[-which(Macros == "Regional Ignorada")] ##### exclui da analise obitos com regional ignorada.

PopMacroMG<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/Populacao/BancoFinalPop1979-2022MGporMacro-incluiBH-PC-MG.csv"), 
                      colClasses = c("character", "character", "character", "character", "integer"), fileEncoding = "Latin1")

Obitos$ANO<-as.character(Obitos$ANO)
Obitos$SEXO<-as.character(Obitos$SEXO)
Obitos$FXETARIA<-as.character(Obitos$FXETARIA)
Obitos$MacroRegiao<-as.character(Obitos$MacroRegiao)

Obitos<-left_join(Obitos, PopMacroMG, by = c("MacroRegiao", "ANO", "SEXO", "FXETARIA"))

Obitos$PopPadrao[Obitos$FXETARIA == "0004"]<-12000
Obitos$PopPadrao[Obitos$FXETARIA == "0509"]<-10000
Obitos$PopPadrao[Obitos$FXETARIA == "1014"]<-9000
Obitos$PopPadrao[Obitos$FXETARIA == "1519"]<-9000
Obitos$PopPadrao[Obitos$FXETARIA == "2024"]<-8000
Obitos$PopPadrao[Obitos$FXETARIA == "2529"]<-8000
Obitos$PopPadrao[Obitos$FXETARIA == "3034"]<-6000
Obitos$PopPadrao[Obitos$FXETARIA == "3539"]<-6000
Obitos$PopPadrao[Obitos$FXETARIA == "4044"]<-6000
Obitos$PopPadrao[Obitos$FXETARIA == "4549"]<-6000
Obitos$PopPadrao[Obitos$FXETARIA == "5054"]<-5000
Obitos$PopPadrao[Obitos$FXETARIA == "5559"]<-4000
Obitos$PopPadrao[Obitos$FXETARIA == "6064"]<-4000
Obitos$PopPadrao[Obitos$FXETARIA == "6569"]<-3000
Obitos$PopPadrao[Obitos$FXETARIA == "7074"]<-2000
Obitos$PopPadrao[Obitos$FXETARIA == "7579"]<-1000
Obitos$PopPadrao[Obitos$FXETARIA == "8099"]<-1000

Obitos<-Obitos|>
        mutate(PopMacro = POPMACRO)

Obitos$POPMACRO<-NULL

Obitos<-Obitos[Obitos$FXETARIA !="9999",] #### remove idade ignorada

Obitos2018_2022<-Obitos|>
        filter(ANO %in% ("2018":"2022"))|>
        group_by(MacroRegiao, SEXO, NEOPLASIA, FXETARIA)|>
        summarize(PopMacro = sum(PopMacro), Count = sum(Count), PopPadrao = sum(PopPadrao))

Obitos2018_2022$ANO<-"Ultimos5anos"
head(Obitos2018_2022)

Obitos<-rbind(Obitos, Obitos2018_2022)

ObitosAmbosSexos<-Obitos|>
       group_by(MacroRegiao, ANO, NEOPLASIA, FXETARIA)|>
       summarize(PopMacro = sum(PopMacro), Count = sum(Count), PopPadrao = sum(PopPadrao))

ObitosAmbosSexos$SEXO<-"Ambos"

Obitos<-rbind(Obitos, ObitosAmbosSexos)

Obitos$TaxaEspecifica <- Obitos$Count/Obitos$PopMacro*100000
Obitos$CasosEsperados <- Obitos$TaxaEspecifica*Obitos$PopPadrao/100000

ObitosNeoplasiaTaxas<-Obitos%>%group_by(MacroRegiao,ANO,SEXO,NEOPLASIA)%>%
                 summarize(Casos = sum(Count),
                           TotalPopMacro = sum(PopMacro),
                           TotalPopPadrao = sum(PopPadrao),
                           TotalCasosEsperados = sum(CasosEsperados))

ObitosNeoplasiaTaxas$TaxaAjust<-ObitosNeoplasiaTaxas$TotalCasosEsperados/ObitosNeoplasiaTaxas$TotalPopPadrao*100000
ObitosNeoplasiaTaxas$TaxaBruta<-ObitosNeoplasiaTaxas$Casos/ObitosNeoplasiaTaxas$TotalPopMacro*100000

write.csv2(ObitosNeoplasiaTaxas, file = "Taxas1979-2022 por Macro, Sexo e Neoplasia.csv", row.names = FALSE,
              fileEncoding = "Latin1")
write.csv2(Obitos, file = "TaxasEspecíficas1979-2022 por Macro, Sexo e Neoplasia.csv", row.names = FALSE,
              fileEncoding = "Latin1")


#ObitosNeoplasiaTaxas<-read.csv2(file = "Taxas1979-2022 por Macro, Sexo e Neoplasia.csv")

##### Exporta Planilhas com taxas brutas
ListaTaxas<-xtabs(TaxaBruta ~ ANO + MacroRegiao + SEXO + NEOPLASIA, ObitosNeoplasiaTaxas)
str(ListaTaxas)

Neoplasias<-levels(as.factor(ObitosNeoplasiaTaxas$NEOPLASIA))
Sexo<-levels(as.factor(ObitosNeoplasiaTaxas$SEXO))
Macroregiao<-levels(as.factor(ObitosNeoplasiaTaxas$MacroRegiao))
for(s in 1:length(Sexo)){
i=1
for(i in 1:length(Neoplasias)){
write.csv2(ListaTaxas[,,SEXO = Sexo[s],NEOPLASIAS = Neoplasias[i]], 
   file = paste0("TaxasBrutas - ", (Sexo[s]), "-", Neoplasias[i], ".csv"), fileEncoding = "Latin1")

}
}

##### Exporta Planilhas com taxas ajustadas
ListaTaxasAjust<-xtabs(TaxaAjust ~ ANO + MacroRegiao + SEXO + NEOPLASIA, ObitosNeoplasiaTaxas)
str(ListaTaxas)

Neoplasias<-levels(as.factor(ObitosNeoplasiaTaxas$NEOPLASIA))
Sexo<-levels(as.factor(ObitosNeoplasiaTaxas$SEXO))
Macroregiao<-levels(as.factor(ObitosNeoplasiaTaxas$MacroRegiao))
for(s in 1:length(Sexo)){
i=1
for(i in 1:length(Neoplasias)){
write.csv2(ListaTaxas[,,SEXO = Sexo[s],NEOPLASIAS = Neoplasias[i]], 
   file = paste0("TaxasAjustadas - ", (Sexo[s]), "-", Neoplasias[i], ".csv"), fileEncoding = "Latin1")

}
}

## Caso queira exportar planilhas organizadas por macroregiao


for(s in 1:length(Sexo)){
for(i in 1:length(Macroregiao)){
write.csv2(ListaTaxas[,MacroRegiao = Macroregiao[i],SEXO = Sexo[s],], file = paste0("TaxasBrutas - ", (Sexo[s]), "-", Macroregiao[i], ".csv"))
}
}
