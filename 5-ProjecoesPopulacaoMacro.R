### Calcula projecoes da populacao para as macroregioes
## Gil Pena
rm(list=ls(all=TRUE))
gc()
Drive<-"D:" #na camg
#Drive<-"E:"
setwd(file.path(Drive, "Estimativas2024MacroCorretas/PopulacaoProjetada"))

if (!require(dplyr)) install.packages("dplyr");library(dplyr)
if (!require(tidyr)) install.packages("tidyr");library(tidyr)
if (!require(readxl)) install.packages("readxl");library(readxl)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

PopMacroMG<-read.csv2(file.path(Drive, "Estimativas2024MacroCorretas/Populacao/BancoFinalPop1979-2022MGporMacro-incluiBH-PC-MG.csv"),
                   fileEncoding = "Latin1")

### Projecao da população 2023 a 2027
#Através da resolução do sistema acima, tem-se que:
#ai = Pi (t1) – Pi (t0) / P (t1) – P( t0 )
#bi = Pi ( t0 ) – ai P ( t0 )

#Pi ( t ) = ai P ( t ) + bi

PopMacroMG$a<-NA
MacroRegiao<-levels(as.factor(PopMacroMG$MacroRegiao))

Projecao<-data.frame()
for(i in 1:length(MacroRegiao)){
a<-(PopMacroMG$POPMACRO[PopMacroMG$ANO == "2022" & PopMacroMG$MacroRegiao == MacroRegiao[i]] - 
    PopMacroMG$POPMACRO[PopMacroMG$ANO == "2010" & PopMacroMG$MacroRegiao == MacroRegiao[i]]) /
   (PopMacroMG$POPMACRO[PopMacroMG$ANO == "2022" & PopMacroMG$MacroRegiao == "Minas Gerais"] -
    PopMacroMG$POPMACRO[PopMacroMG$ANO == "2010" & PopMacroMG$MacroRegiao == "Minas Gerais"])

b<-PopMacroMG$POPMACRO[PopMacroMG$ANO == "2010" & PopMacroMG$MacroRegiao == MacroRegiao[i]]-
  (a*PopMacroMG$POPMACRO[PopMacroMG$ANO == "2010" & PopMacroMG$MacroRegiao == "Minas Gerais"])

Projecao1<-data.frame(SEXO = PopMacroMG$SEXO[PopMacroMG$ANO == "2022" & PopMacroMG$MacroRegiao == MacroRegiao[i]],
                      FXETARIA = PopMacroMG$FXETARIA[PopMacroMG$ANO == "2022" & PopMacroMG$MacroRegiao == MacroRegiao[i]],  
                      a = a, b = b)

Projecao1$MacroRegiao<-MacroRegiao[i]

Projecao<-rbind(Projecao, Projecao1)

}

PopMG2023<-c(673088, 681460, 670716, 712673,800485,859044,853022,852592,816538,729228,648996,613020,539424,
433540,315157,209394,(128529+70905+42299),641799,650488,640620,681868,770339,840554,843936,852783,836560,
765273,695563,668281,603460,497442,376642,263532,(174368+104097+76975))
 
PopMG2024<-c(
666961,682469,672231,698459,777406,856088,850078,853479,
824586,748886,652670,618094,549984,448782,328675,219591,(132929+73961+44750),635931,
651389,641927,668015,746799,834803,841214,850787,
842369,782928,699589,672555,615996,514497,393033,276038,(179910+108544+81072))

PopMG2025<-c(
660153,679955,678119,684795,757129,847568,848420,853142,
831338,765978,661090,621780,559407,463044,342644,230099,(138300+76756+47247),
629417,648931,647436,654698,726308,823837,838979,848448,
846502,798012,707745,675886,626582,530850,409724,289128,(186750+112621+85353))

PopMG2026<-c(
652773,679925,679413,675059,738023,834744,847985,851617,
836629,780736,673863,624145,567772,476353,356947,240920,(144515+79409+49849),
622360,648849,648578,645154,707516,808653,837191,845800,
848806,810746,719720,678267,635352,546480,426563,302786,(194736+116479+89877))

PopMG2027<-c(
644980,679010,679258,671374,721362,814073,849246,848766,
840459,792913,691193,624882,575291,488661,371539,251970,(151549+81959+52630),
614909,647936,648344,641442,691156,786404,836184,842836,
849188,820985,735703,679396,642466,561413,443398,316934,(203824+120157+94752))

Projecao$Ano2023<-round((Projecao$a*PopMG2023 + Projecao$b),0)
Projecao$Ano2024<-round((Projecao$a*PopMG2024 + Projecao$b),0)
Projecao$Ano2025<-round((Projecao$a*PopMG2025 + Projecao$b),0)
Projecao$Ano2026<-round((Projecao$a*PopMG2026 + Projecao$b),0)
Projecao$Ano2027<-round((Projecao$a*PopMG2027 + Projecao$b),0)

head(Projecao)

ProjecaoMacro<-Projecao|>group_by(MacroRegiao, SEXO)|>
                  summarize(Ano2023 = sum(Ano2023),
                            Ano2024 = sum(Ano2024),   
                            Ano2025 = sum(Ano2025),
                            Ano2026 = sum(Ano2026),
                            Ano2027 = sum(Ano2027))

ProjecaoMacro$SEXO[ProjecaoMacro$SEXO == "1"]<-"Sexo Masculino"
ProjecaoMacro$SEXO[ProjecaoMacro$SEXO == "2"]<-"Sexo Feminino"


ProjecaoMacroT<-Projecao|>group_by(MacroRegiao)|>
                  summarize(Ano2023 = sum(Ano2023),
                            Ano2024 = sum(Ano2024),   
                            Ano2025 = sum(Ano2025),
                            Ano2026 = sum(Ano2026),
                            Ano2027 = sum(Ano2027))

ProjecaoMacroT$SEXO<-"Ambos"
ProjecaoMacro<-rbind(ProjecaoMacro, ProjecaoMacroT)
ProjecaoMacro<-ProjecaoMacro|>
               arrange(MacroRegiao)

write.csv2(ProjecaoMacro, file = "ProjecaoPopulacaoMacro.csv", row.names = FALSE, fileEncoding = "Latin1")
