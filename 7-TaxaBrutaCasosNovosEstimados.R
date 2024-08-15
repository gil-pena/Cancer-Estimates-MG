## Prepara a planilha para as taxas brutas e numero de casos novos
## Gil
rm(list=ls(all=TRUE))
gc()
#Drive<-"D:" #na camg
Drive<-"E:"

setwd(file.path(Drive, "Estimativas2024MacroCorretas/TaxaBrutaCasosNovosEstimados"))

if (!require(dplyr)) install.packages("dplyr", lib = file.path(Drive, "R-4.3.2/library"));library(dplyr)
if (!require(tidyr)) install.packages("tidyr", lib = file.path(Drive, "R-4.3.2/library"));library(tidyr)
if (!require(readxl)) install.packages("readxl", lib = file.path(Drive, "R-4.3.2/library"));library(readxl)
if(!require(ggplot2)) install.packages("ggplot2", lib = file.path(Drive, "R-4.3.2/library")); library(ggplot2)

#### Planilha de taxas brutas projetadas:
TaxasBrutasProj<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/RegressoesTxBruta/TaxasBrutasProjetadas.csv"), fileEncoding = "Latin1")
head(TaxasBrutasProj)
#### Planilha da Razão Incidência/Mortalidade
RazaoIncidMortal<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/RazaoIncidMortal/RazaoIncidMortal.csv"), fileEncoding = "Latin1")
head(RazaoIncidMortal)
RazaoIncidMortal<-RazaoIncidMortal|>
     select(NEOPLASIA, RazaoIM_Masc, RazaoIM_Fem, RazaoIM_Total)

RazaoIncidMortal_L<-pivot_longer(RazaoIncidMortal, cols = starts_with("RazaoIM_"), names_to = "SEXO", names_prefix = "RazaoIM_", values_to = "RazaoIM")

RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Masc"]<-"Sexo Masculino"
RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Fem"]<-"Sexo Feminino"
RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Total"]<-"Ambos os sexos"

#### Planilha da populacao projetada
ProjecaoPopulacaoMacro<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/PopulacaoProjetada/ProjecaoPopulacaoMacro.csv"), fileEncoding = "Latin1")
str(ProjecaoPopulacaoMacro)

ProjecaoPopulacaoMacro$SEXO[ProjecaoPopulacaoMacro$SEXO == "Ambos"]<-"Ambos os sexos"

head(ProjecaoPopulacaoMacro)
ProjecaoPopulacaoMacro<-ProjecaoPopulacaoMacro|>
             select(MacroRegiao, SEXO, Ano2024)

TaxaBrutaProjA<-left_join(TaxasBrutasProj, RazaoIncidMortal_L, by = c("Neoplasia" = "NEOPLASIA", "Sexo" = "SEXO"))

TaxaBrutaProjA<-TaxaBrutaProjA|>
           filter(!is.na(RazaoIM))

TaxaBrutaProjA$TaxaIncidEstim[TaxaBrutaProjA$rsquare >= 0.7]<-TaxaBrutaProjA$TaxaBrutaPrev[TaxaBrutaProjA$rsquare >= 0.7]*TaxaBrutaProjA$RazaoIM[TaxaBrutaProjA$rsquare >= 0.7]
TaxaBrutaProjA$TaxaIncidEstim[TaxaBrutaProjA$rsquare < 0.7]<-TaxaBrutaProjA$TaxaBrutaMedia[TaxaBrutaProjA$rsquare < 0.7]*TaxaBrutaProjA$RazaoIM[TaxaBrutaProjA$rsquare < 0.7]

TaxaBrutaProjB<-left_join(TaxaBrutaProjA, ProjecaoPopulacaoMacro, by = c("Macro" = "MacroRegiao", "Sexo" = "SEXO"))

str(TaxaBrutaProjB)

TaxaBrutaProjB$NumCasosNovos2024<-TaxaBrutaProjB$TaxaIncidEstim*TaxaBrutaProjB$Ano2024/100000

# =SEERRO(SE(E(G39<16;G39>0);10;SE((MOD(G39;5)=0);SE(MOD(G39;10)=0;G39;G39-5);ARRED((G39/10);0)*10));"-")

TaxaBrutaProjB$NumCasosNovos2024arred <- ifelse(TaxaBrutaProjB$NumCasosNovos2024 < 20 & TaxaBrutaProjB$NumCasosNovos2024 > 0, "**", 
                                           ifelse(TaxaBrutaProjB$NumCasosNovos2024 >=20 & (TaxaBrutaProjB$NumCasosNovos2024 %% 5) == 0,
                                             ifelse(TaxaBrutaProjB$NumCasosNovos2024 >=20 & (TaxaBrutaProjB$NumCasosNovos2024 %% 10) == 0,
                                                 TaxaBrutaProjB$NumCasosNovos2024, TaxaBrutaProjB$NumCasosNovos2024 - 5),
                                                    round(TaxaBrutaProjB$NumCasosNovos2024 / 10) * 10))

write.csv2(TaxaBrutaProjB, "TaxaBrutaComRazaoIMCasosNovosArred.csv", row.names = FALSE, fileEncoding = "Latin1")
###################################

TaxaBrutaProjMinasGerais<-TaxaBrutaProjB|>
                 filter(Macro == "Minas Gerais")|>
                 select(Sexo, Neoplasia, NumCasosNovos2024arred, TaxaIncidEstim)

Neoplasia<-data.frame(Neoplasia = TaxaBrutaProjMinasGerais$Neoplasia[TaxaBrutaProjMinasGerais$Sexo == "Ambos os sexos"])

TaxaBrutaProjMinasGeraisL<-Neoplasia|>
                           left_join(TaxaBrutaProjMinasGerais[TaxaBrutaProjMinasGerais$Sexo == "Sexo Masculino",])|>
                           rename(Homens_Casos = NumCasosNovos2024arred, Homens_TaxaBruta = TaxaIncidEstim)
TaxaBrutaProjMinasGeraisL$Sexo<-NULL

TaxaBrutaProjMinasGeraisL<-TaxaBrutaProjMinasGeraisL|>
                           left_join(TaxaBrutaProjMinasGerais[TaxaBrutaProjMinasGerais$Sexo == "Sexo Feminino",], by = c("Neoplasia"))|>
                           rename(Mulheres_Casos = NumCasosNovos2024arred, Mulheres_TaxaBruta = TaxaIncidEstim)
TaxaBrutaProjMinasGeraisL$Sexo<-NULL

TaxaBrutaProjMinasGeraisL<-TaxaBrutaProjMinasGeraisL|>
                           left_join(TaxaBrutaProjMinasGerais[TaxaBrutaProjMinasGerais$Sexo == "Ambos os sexos",], by = c("Neoplasia"))|>
                           rename(Total_Casos = NumCasosNovos2024arred, Total_TaxaBruta = TaxaIncidEstim)
TaxaBrutaProjMinasGeraisL$Sexo<-NULL



TaxaBrutaProjMinasGeraisL<-(Homens_Casos = TaxaBrutaProjMinasGerais$NumCasosNovos2024arred[TaxaBrutaProjMinasGerais$Sexo == "Sexo Masculino"],
                                     Homens_TxBruta = TaxaBrutaProjMinasGerais$TaxaIncidEstim[TaxaBrutaProjMinasGerais$Sexo == "Sexo Masculino"],
                                     Mulheres_Casos = TaxaBrutaProjMinasGerais$NumCasosNovos2024arred[TaxaBrutaProjMinasGerais$Sexo == "Sexo Feminino"],
                                     Mulheres_TxBruta = TaxaBrutaProjMinasGerais$TaxaIncidEstim[TaxaBrutaProjMinasGerais$Sexo == "Sexo Feminino"],
                                     Total_Casos = TaxaBrutaProjMinasGerais$NumCasosNovos2024arred[TaxaBrutaProjMinasGerais$Sexo == "Ambos os sexos"],
                                     Total_TxBruta = TaxaBrutaProjMinasGerais$TaxaIncidEstim[TaxaBrutaProjMinasGerais$Sexo == "Ambos os sexos"])
        
write.csv2(TaxaBrutaProjMinasGeraisL, "EstimativasMG-2024.csv", row.names = FALSE)


write.csv2(TaxaBrutaProjB, file="TaxaBrutaComRazaoIMeCasosNovos1.csv", row.names = FALSE, fileEncoding = "Latin1")

