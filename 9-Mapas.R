##Prepara o mapa das macrorregioes de acordo com a tabela dos municípios
## Gil
rm(list=ls(all=TRUE))
gc()
#Drive<-"D:" #na camg
Drive<-"E:"
setwd(file.path(Drive, "Estimativas2024/2024-02-26"))

if (!require(dplyr)) install.packages("dplyr");library(dplyr)
if (!require(tidyr)) install.packages("tidyr");library(tidyr)
if (!require(readxl)) install.packages("readxl");library(readxl)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(sf)) install.packages("sf"); library (sf)

Municipios<-read_sf(file.path(Drive, "Georreferenciamento/Formato .dbf; .prj; .qpj; .shp; .shx/3 Municípios/31MUE250GC_SIR.shp"))
Municipios$CD_GEOCMU<-as.integer(substr(Municipios$CD_GEOCMU,1,6))
MunicipiosReg<-read.csv2(file.path(Drive, "TabelasAux/TABELA DE MUNICÍPIOS PDR 2024-Rita-SINAN.csv"),
              fileEncoding = "Latin1")
MacroRegiao<-MunicipiosReg|>
          select(CÓDIGO, REGIÃO.AMPLIADA.DE.SAÚDE)|>
          rename(Cod_Munic = CÓDIGO, MacroRegiao = REGIÃO.AMPLIADA.DE.SAÚDE)


Municipios1<-left_join(Municipios, MacroRegiao, by = c("CD_GEOCMU" = "Cod_Munic"))


Macros<-levels(as.factor(Municipios1$MacroRegiao))

Mapa<-data.frame()
for(i in 1:length(Macros)){
geometria<-st_union(Municipios1[Municipios1$MacroRegiao == Macros[i],])
MapaProv<-data.frame(MacroRegiao = Macros[i])
st_geometry(MapaProv)<-geometria
Mapa<-rbind(Mapa, MapaProv)
}
st_write(Mapa, "MapaMGMacros.shp")

plot(Mapa, key.pos = 4, pal = sf.colors(16))

TaxasAjustEst<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/TaxaAjustadaEstimada/TaxaAjustEstimada.csv"), 
                           fileEncoding = "Latin1")

str(TaxasAjustEst)

TaxasAjustEst<-TaxasAjustEst|>
            select(Macro, Sexo, Neoplasia, TaxaIncidEstim)

CavidadeOral<-left_join(Mapa, TaxasAjustEst[TaxasAjustEst$Sexo == "Ambos os sexos" & 
                                                   TaxasAjustEst$Neoplasia == "01-Cavidade Oral (C00-C10)",],
                        by=c("MacroRegiao" = "Macro"))
plot(CavidadeOral[4], pal = hcl.colors(n=4, palette = "Greens 3"), nbreaks = 4, breaks = "quantile")
plot(CavidadeOral[4], main = paste("Cavidade oral", "-", "Ambos os sexos"), key.pos = 1,
                     pal = hcl.colors(n=4, palette = "Greens 3", rev = TRUE), nbreaks = 4, breaks = "quantile")

str(CavidadeOral)

plot(CavidadeOral[5])


