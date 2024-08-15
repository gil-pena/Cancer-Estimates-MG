## Prepara as planilhas para as taxas ajustadas estimadas
## Gil
rm(list=ls(all=TRUE))
gc()
#Drive<-"D:" #na camg
Drive<-"E:"

setwd(file.path(Drive, "Estimativas2024MacroCorretas/TaxaAjustadaEstimada"))

if (!require(dplyr)) install.packages("dplyr", lib = file.path(Drive, "R-4.3.2/library"));library(dplyr)
if (!require(tidyr)) install.packages("tidyr", lib = file.path(Drive, "R-4.3.2/library"));library(tidyr)
if (!require(readxl)) install.packages("readxl", lib = file.path(Drive, "R-4.3.2/library"));library(readxl)
if(!require(ggplot2)) install.packages("ggplot2", lib = file.path(Drive, "R-4.3.2/library")); library(ggplot2)

#### Planilha de taxas brutas projetadas:
TaxasAjustProj<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/RegressoesTxAjust/TaxasAjustProjetadas.csv"), fileEncoding = "Latin1")
head(TaxasAjustProj)
#### Planilha da Razão Incidência/Mortalidade
RazaoIncidMortal<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/RazaoIncidMortal/RazaoIncidMortal.csv"), fileEncoding = "Latin1")
head(RazaoIncidMortal)
RazaoIncidMortal<-RazaoIncidMortal|>
     select(NEOPLASIA, RazaoIM_Masc, RazaoIM_Fem, RazaoIM_Total)

RazaoIncidMortal_L<-pivot_longer(RazaoIncidMortal, cols = starts_with("RazaoIM_"), names_to = "SEXO", names_prefix = "RazaoIM_", values_to = "RazaoIM")
head(RazaoIncidMortal_L)
RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Masc"]<-"Sexo Masculino"
RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Fem"]<-"Sexo Feminino"
RazaoIncidMortal_L$SEXO[RazaoIncidMortal_L$SEXO == "Total"]<-"Ambos os sexos"

TaxaAjustProjA<-left_join(TaxasAjustProj, RazaoIncidMortal_L, by = c("Neoplasia" = "NEOPLASIA", "Sexo" = "SEXO"))

TaxaAjustProjA<-TaxaAjustProjA|>
           filter(!is.na(RazaoIM))

TaxaAjustProjA$TaxaIncidEstim[TaxaAjustProjA$rsquare >= 0.7]<-
                          TaxaAjustProjA$TaxaAjustPrev2024[TaxaAjustProjA$rsquare >= 0.7]*
                          TaxaAjustProjA$RazaoIM[TaxaAjustProjA$rsquare >= 0.7]
TaxaAjustProjA$TaxaIncidEstim[TaxaAjustProjA$rsquare < 0.7]<-TaxaAjustProjA$TaxaAjustMedia[TaxaAjustProjA$rsquare < 0.7]*TaxaAjustProjA$RazaoIM[TaxaAjustProjA$rsquare < 0.7]

str(TaxaAjustProjA)

write.csv2(TaxaAjustProjA, file="TaxaAjustEstimada.csv", row.names = FALSE, fileEncoding = "Latin1")
##################################################################################################
###### Preparar mapas

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


write.csv2(TaxaBrutaProjB, file="TaxaAjustEstimada.csv", row.names = FALSE, fileEncoding = "Latin1")

