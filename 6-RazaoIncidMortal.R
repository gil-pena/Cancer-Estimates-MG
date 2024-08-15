##Calcula a razao da incidencia mortalidade (RCBP BH e Poços de Caldas)
## Gil Pena
rm(list=ls(all=TRUE))
gc()
#Drive<-"D:" #na camg
Drive<-"E:"
setwd(file.path(Drive, "Estimativas2024MacroCorretas/RazaoIncidMortal"))

if (!require(dplyr)) install.packages("dplyr");library(dplyr)
if (!require(tidyr)) install.packages("tidyr");library(tidyr)
if (!require(readxl)) install.packages("readxl");library(readxl)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

##### Organiza os arquivos de incidencia
IncidBHMasc<-read.csv2(file.path(Drive, "Estimativas2024/RazaoIncidMort/resultadoBHMasc.csv"), 
                       skip = 4,
                       fileEncoding = "Latin1")
str(IncidBHMasc)
colnames(IncidBHMasc)<-c("CID10", "FXETARIA9999", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", 
                         "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                         "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", 
                         "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
                         "FXETARIA7579", "FXETARIA8084", "FXETARIA8599", "TOTAL")

IncidBHMasc$SEXO<-"1"
IncidBHMasc$RCBP<-"BH"

IncidBHFem<-read.csv2(file.path(Drive, "Estimativas2024/RazaoIncidMort/resultadoBHFem.csv"), 
                       skip = 4,
                       fileEncoding = "Latin1")
str(IncidBHFem)
colnames(IncidBHFem)<-c("CID10", "FXETARIA9999", "FXETARIA0004", "FXETARIA0509", "FXETARIA1014", 
                         "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                         "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", 
                         "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
                         "FXETARIA7579", "FXETARIA8084", "FXETARIA8599", "TOTAL")
IncidBHFem$SEXO<-"2"
IncidBHFem$RCBP<-"BH"

IncidPCMasc<-read.csv2(file.path(Drive, "Estimativas2024/RazaoIncidMort/resultadoPCMasc.csv"), 
                       skip = 4,
                       fileEncoding = "Latin1")
str(IncidPCMasc)
colnames(IncidPCMasc)<-c("CID10", "FXETARIA0004", "FXETARIA1014", 
                         "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                         "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", 
                         "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
                         "FXETARIA7579", "FXETARIA8084", "FXETARIA8599", "TOTAL")

IncidPCMasc$FXETARIA9999<-0L
IncidPCMasc$FXETARIA0509<-0L
IncidPCMasc$SEXO<-"1"
IncidPCMasc$RCBP<-"PC"

IncidPCFem<-read.csv2(file.path(Drive, "Estimativas2024/RazaoIncidMort/resultadoPCFem.csv"), 
                       skip = 4,
                       fileEncoding = "Latin1")

str(IncidPCFem)
colnames(IncidPCFem)<-c("CID10", "FXETARIA0004", "FXETARIA1014", 
                         "FXETARIA1519", "FXETARIA2024", "FXETARIA2529", "FXETARIA3034", 
                         "FXETARIA3539", "FXETARIA4044", "FXETARIA4549", "FXETARIA5054", 
                         "FXETARIA5559", "FXETARIA6064", "FXETARIA6569", "FXETARIA7074", 
                         "FXETARIA7579", "FXETARIA8084", "FXETARIA8599", "TOTAL")

IncidPCFem$FXETARIA9999<-0L
IncidPCFem$FXETARIA0509<-0L
IncidPCFem$SEXO<-"2"
IncidPCFem$RCBP<-"PC"

Incid<-rbind(IncidBHMasc, IncidBHFem, IncidPCMasc, IncidPCFem)
Incid$TOTAL<-NULL

Incid<-pivot_longer(
           Incid, 
           cols = starts_with("FXETARIA"),
           names_to = "FXETARIA",
           names_prefix = "FXETARIA",
           values_to = "CASOS")

Incid$NEOPLASIA<-NA
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10")] <-"01-Cavidade Oral (C00-C10)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C15")] <-"02-Esofago (C15)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C16")] <-"03-Estomago(C16)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C18", "C19", "C20", "C21")] <- "04-Colon e Reto (C18-C21)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C22")] <- "05-Figado e vias biliares intra-hepaticas (C22)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C25")] <- "06-Pancreas (C25)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C32")] <- "07-Laringe (C32)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C33", "C34")] <- "08-Traqueia, bronquio e pulmao (C33-C34)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C43")] <- "09-Melanoma maligno da pele (C43)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C44")] <- "10-Outras neoplasias malignas da pele (C44)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C50")] <- "11-Mama feminina (C50)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C53")] <- "12-Colo do Utero (C53)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C54")] <- "13-Corpo do Utero (C54)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C55")] <- "Utero SOE (C55)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C56")] <- "14-Ovario (C56)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C61")] <- "15-Prostata (C61)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C67")] <- "16-Bexiga (C67)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C70", "C71", "C72")] <- "17-Sistema nervoso central (C70-C72)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C73")] <- "18-Glandula Tireoide (C73)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C81")] <- "19-Linfoma de Hodgkin (C81)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C82", "C83", "C84", "C85", "C96")] <- "20-Linfoma nao Hodgkin (C82-C85; C96)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C91", "C92", "C93", "C94", "C95")] <- "21-Leucemias (C91-C95)"
Incid$NEOPLASIA[substr(Incid$CID10, 1, 3) %in% c("C11", "C12", "C13", "C14", "C17", "C23", "C24", "C26", "C27", "C28", 
                                                  "C29", "C30", "C31", "C35", "C36", "C37", "C38", "C39", "C40",
                                                  "C41", "C45", "C46", "C47", "C48", "C49", "C51", "C52",
                                                  "C57", "C58", "C59", "C60", "C62", "C63", "C64", "C65", "C66",
                                                  "C68", "C69", "C74", "C75", "C76", "C77", "C78", "C79", "C80",
                                                  "C86", "C88", "C90", "C97", "D46")] <- "22-Outras neoplasias (C00-C97; D46)"

Incid<-Incid[!is.na(Incid$NEOPLASIA),]

###### Realocação dos CASOS C55
RCBP<-levels(as.factor(Incid$RCBP))
Realocacao<-data.frame()

r=1
for(r in r:length(RCBP)){

C53<-sum(Incid$CASOS[Incid$RCBP == RCBP[r] &
                 Incid$NEOPLASIA == "12-Colo do Utero (C53)"])

C54<-sum(Incid$CASOS[Incid$RCBP == RCBP[r] & 
                 Incid$NEOPLASIA == "13-Corpo do Utero (C54)"])

C55<-sum(Incid$CASOS[Incid$RCBP == RCBP[r] &
                 Incid$NEOPLASIA == "Utero SOE (C55)"])
P53<-C53/(C53+C54)


Realocacao<-rbind(Realocacao, data.frame(
                              RCBP[r],
                              C53,
                              C54,
                              C55,
                              P53))

}
colnames(Realocacao)<- c("RCBP", "C53", "C54", "C55", "P53")

Realocacao$P53[is.na(Realocacao$P53)]<-0.5 #### coloca resultado de divisao 0/0 como 0,5.


head(Realocacao)
#write.csv2(Realocacao, "C55C54C53.csv", row.names = FALSE)

PropC55<-sum(Realocacao$C55)/(sum(Realocacao$C53)+sum(Realocacao$C54)+sum(Realocacao$C55))
PropC55
#######[1] 0.02855659
#######(proporção excessiva, de acordo com artigo das cabras e ovelhas

IncidRealocados<-Incid
RCBP<-levels(as.factor(IncidRealocados$RCBP))
IncidRealocados$Adiciona<-0
IncidRealocados$Idade<-ifelse(test = as.character(IncidRealocados$FXETARIA) < "5054", "Menor50", "Maior50")



r=1
for(r in r:length(RCBP)){
######De 0 a 49 anos -> C53*=C53+C54+C55 (planilha do INCA)
###### estou em dúvida no procedimento, vou alocar apenas os C55

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "12-Colo do Utero (C53)" &
                         IncidRealocados$RCBP == "BH" &
                         IncidRealocados$Idade == "Menor50"]<- 
IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                         IncidRealocados$RCBP == "BH" &
                         IncidRealocados$Idade == "Menor50"]

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                            IncidRealocados$RCBP == RCBP[r] &
                            IncidRealocados$Idade == "Menor50"]<- 
                         IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                            IncidRealocados$RCBP == RCBP[r] &
                            IncidRealocados$Idade == "Menor50"] -
                         IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                            IncidRealocados$RCBP == RCBP[r] &
                            IncidRealocados$Idade == "Menor50"]

##### Acima de 50 anos -> C53**= C53+(C55*pC53)

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "12-Colo do Utero (C53)" &
                         IncidRealocados$RCBP == RCBP[r] &
                         IncidRealocados$Idade == "Maior50"] <- 
round((IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                                  IncidRealocados$RCBP == RCBP[r] &
                                  IncidRealocados$Idade == "Maior50"]*
                                Realocacao$P53[Realocacao$RCBP == RCBP[r]]),0)

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE(C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"] <- 
                         IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"]-
                         round((IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"]*
                           Realocacao$P53[Realocacao$RCBP == RCBP[r]]),0)

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "13-Corpo do Utero (C54)" &
                           IncidRealocados$RCBP == RCBP[r]&
                           IncidRealocados$Idade == "Maior50"] <- 
                         round((IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"] *
                           (1-Realocacao$P53[Realocacao$RCBP == RCBP[r]])),0)

IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r]&
                           IncidRealocados$Idade == "Maior50"] <- 
                         IncidRealocados$Adiciona[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"]-
                         round((IncidRealocados$CASOS[IncidRealocados$NEOPLASIA == "Utero SOE (C55)" &
                           IncidRealocados$RCBP == RCBP[r] &
                           IncidRealocados$Idade == "Maior50"] *
                           (1-Realocacao$P53[Realocacao$RCBP == RCBP[r]])),0)

}

write.csv2(IncidRealocados, file = "IncidR.csv", row.names = FALSE)

IncidRealocados$CASOS <- IncidRealocados$CASOS + IncidRealocados$Adiciona

IncidRealocados$Adiciona<-NULL
IncidRealocados$Idade<-NULL



head(IncidRealocados)
####################################

Incid<-IncidRealocados
#####
Incid$SEXO[Incid$SEXO == "1"]<-"Masc"
Incid$SEXO[Incid$SEXO == "2"]<-"Fem"

Incid<-Incid[Incid$FXETARIA != "9999",]
Incid<-Incid[Incid$NEOPLASIA !="Utero SOE (C55)",]

Incid<-Incid|>
        select(RCBP, NEOPLASIA, SEXO, FXETARIA, CASOS)        

Incid$CASOS[is.na(Incid$CASOS)]<-0L

Incid<-Incid|>
         group_by(RCBP, NEOPLASIA, SEXO)|>
         summarize(CASOS = sum(CASOS))

Incidw<-Incid|>
        pivot_wider(names_from = SEXO, values_from = CASOS)

Incidw$Fem[is.na(Incidw$Fem)]<-0L
Incidw$Masc[is.na(Incidw$Masc)]<-0L


Incidw$Total<-Incidw$Masc + Incidw$Fem 


TodasNeoplasias<-Incidw|>
         group_by(RCBP)|>
         summarize(Masc = sum(Masc),
                   Fem = sum(Fem),
                   Total = sum(Total)) 


TodasNeoplasias$NEOPLASIA<-"TODAS NEOPLASIAS"      

IncidFinal<-rbind(Incidw, TodasNeoplasias)
str(IncidFinal)
IncidFinal<-IncidFinal|>
            select(RCBP, NEOPLASIA, Masc, Fem, Total)

##Prepara população BH - 2015 a 2019
##Prepara população PC - 2010 a 2014

Populacao<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/Populacao/BancoFinalPop1979-2022MGporMacro-incluiBH-PC-MG.csv"),
                     fileEncoding = "Latin1")

POPBH2015_2019<-Populacao|>
          filter(ANO %in% ("2015":"2019") & MacroRegiao == "Belo Horizonte")|>
          group_by(SEXO)|>
          summarize(POPULACAO = sum(POPMACRO))

POPBH2015_2019<-data.frame(Masc = POPBH2015_2019$POPULACAO[POPBH2015_2019$SEXO =="1"],
                           Fem = POPBH2015_2019$POPULACAO[POPBH2015_2019$SEXO =="2"])
POPBH2015_2019$Total<-POPBH2015_2019$Masc+POPBH2015_2019$Fem

##### Poços de Caldas

POPPC2010_2014<-Populacao|>
          filter(ANO %in% ("2010":"2014") & MacroRegiao == "Poços de Caldas")|>
          group_by(SEXO)|>
          summarize(POPULACAO = sum(POPMACRO))

POPPC2010_2014<-data.frame(Masc = POPPC2010_2014$POPULACAO[POPPC2010_2014$SEXO =="1"],
                           Fem = POPPC2010_2014$POPULACAO[POPPC2010_2014$SEXO =="2"])
POPPC2010_2014$Total<-POPPC2010_2014$Masc+POPPC2010_2014$Fem

##########Preparando as bases de mortalidade

Obitos<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/Mortalidade/Taxas1979-2022 por Macro, Sexo e Neoplasia.csv"),
                     fileEncoding = "Latin1")

MortalPC2010_2014<-Obitos|>
                  filter(ANO %in% (2010:2014) & MacroRegiao == "Poços de Caldas")|>
                  group_by(SEXO, NEOPLASIA)|>
                  summarize(CASOS = sum(Casos))

MortalPC2010_2014w<-pivot_wider(MortalPC2010_2014, names_from = SEXO, values_from = CASOS)
str(MortalPC2010_2014w)
MortalPC2010_2014w<-MortalPC2010_2014w|>
                     rename("Masc" = "1", "Fem" = "2", "Total" = "Ambos")

MortalBH2015_2019<-Obitos|>
                  filter(ANO %in% (2015:2019) & MacroRegiao == "Belo Horizonte")|>
                  group_by(SEXO, NEOPLASIA)|>
                  summarize(CASOS = sum(Casos))

MortalBH2015_2019w<-pivot_wider(MortalBH2015_2019, names_from = SEXO, values_from = CASOS)
str(MortalBH2015_2019w)
MortalBH2015_2019w<-MortalBH2015_2019w|>
                     rename("Masc" = "1", "Fem" = "2", "Total" = "Ambos")

Mortalidade<-left_join(MortalBH2015_2019w, MortalPC2010_2014w, by = c("NEOPLASIA"))
str(Mortalidade)
Mortalidade<-Mortalidade|>
                 rename("ObitosBH_Masc" = "Masc.x", 
                        "ObitosBH_Fem" = "Fem.x",
                        "ObitosBH_Total" = "Total.x", 
                        "ObitosPC_Masc" = "Masc.y", 
                        "ObitosPC_Fem" = "Fem.y",
                        "ObitosPC_Total" = "Total.y") 

Mortalidade<-left_join(Mortalidade, IncidFinal[IncidFinal$RCBP == "BH",], by = c("NEOPLASIA"))
str(Mortalidade)
Mortalidade$RCBP<-NULL
Mortalidade<-Mortalidade|>
                 rename("CasosBH_Masc" = "Masc",
                        "CasosBH_Fem" = "Fem", 
                        "CasosBH_Total" = "Total") 

Mortalidade<-left_join(Mortalidade, IncidFinal[IncidFinal$RCBP == "PC",], by = c("NEOPLASIA"))
Mortalidade$RCBP<-NULL
str(Mortalidade)
Mortalidade<-Mortalidade|>
                 rename("CasosPC_Masc" = "Masc",
                        "CasosPC_Fem" = "Fem", 
                        "CasosPC_Total" = "Total") 

Mortalidade$PopBH_Masc<-POPBH2015_2019$Masc
Mortalidade$PopBH_Fem<-POPBH2015_2019$Fem
Mortalidade$PopBH_Total<-POPBH2015_2019$Total

Mortalidade$PopPC_Masc<-POPPC2010_2014$Masc
Mortalidade$PopPC_Fem<-POPPC2010_2014$Fem
Mortalidade$PopPC_Total<-POPPC2010_2014$Total

Mortalidade$RazaoIM_BH_Masc<-(Mortalidade$CasosBH_Masc/sqrt(Mortalidade$PopBH_Masc))/
                             (Mortalidade$ObitosBH_Masc/sqrt(Mortalidade$PopBH_Masc))

Mortalidade$RazaoIM_BH_Fem<-(Mortalidade$CasosBH_Fem/sqrt(Mortalidade$PopBH_Fem))/
                             (Mortalidade$ObitosBH_Fem/sqrt(Mortalidade$PopBH_Fem))

Mortalidade$RazaoIM_BH_Total<-(Mortalidade$CasosBH_Total/sqrt(Mortalidade$PopBH_Total))/
                             (Mortalidade$ObitosBH_Total/sqrt(Mortalidade$PopBH_Total))

Mortalidade$RazaoIM_PC_Masc<-(Mortalidade$CasosPC_Masc/sqrt(Mortalidade$PopPC_Masc))/
                             (Mortalidade$ObitosPC_Masc/sqrt(Mortalidade$PopPC_Masc))

Mortalidade$RazaoIM_PC_Fem<-(Mortalidade$CasosPC_Fem/sqrt(Mortalidade$PopPC_Fem))/
                             (Mortalidade$ObitosPC_Fem/sqrt(Mortalidade$PopPC_Fem))

Mortalidade$RazaoIM_PC_Total<-(Mortalidade$CasosPC_Total/sqrt(Mortalidade$PopPC_Total))/
                             (Mortalidade$ObitosPC_Total/sqrt(Mortalidade$PopPC_Total))

Mortalidade$RazaoIM_Masc<-(Mortalidade$RazaoIM_BH_Masc + Mortalidade$RazaoIM_PC_Masc)/2
Mortalidade$RazaoIM_Fem<-(Mortalidade$RazaoIM_BH_Fem + Mortalidade$RazaoIM_PC_Fem)/2
Mortalidade$RazaoIM_Total<-(Mortalidade$RazaoIM_BH_Total + Mortalidade$RazaoIM_PC_Total)/2

write.csv2(Mortalidade, "RazaoIncidMortal.csv", row.names = FALSE)
#################