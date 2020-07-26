pacman::p_load(dplyr, #manipulacion de datos
               sjPlot, #tablas
               haven, #abrir base .sav
               car, #modificar base
               summarytools, #estadisticos descriptivos
               fastDummies, # Crear variable dummy
               sjlabelled, #etiquetas variables
               ggplot2, #graficos
               coefplot # graficos de coeficientes
)

#abrir base de datos
desiguales <- read_spss("PNUD_DES_2016_publica.sav")

#selección variables primer intento
base <- dplyr::select(desiguales,V13,V15,V128,V140)

#eliminar casos perdidos
base$V13 <- recode(base$V13, "88=NA")
base$V15 <- recode(base$V15, "c(88,99)=NA")
base$V128 <- recode(base$V128, "c(88,99)=NA")
base$V140 <- recode(base$V140, "c(88,99)=NA")

#estadísticos descriptivos base
view(dfSummary(base, headings = FALSE, method = "render"))

#correlación en base
tab_corr(base)

#selección variables segundo intento
base2 <- dplyr::select(desiguales,V14,V142,V128,V140)

#eliminar casos perdidos
base2$V14 <- recode(base2$V14, "88=NA")
base2$V142 <- recode(base2$V142, "c(8,9)=NA")
base2$V128 <- recode(base2$V128, "c(88,99)=NA")
base2$V140 <- recode(base2$V140, "c(88,99)=NA")

#estadísticos descriptivos base 2
view(dfSummary(base2, headings = FALSE, method = "render"))

#correlación en base
tab_corr(base2)

#selección varaibles tercer intento
base3 <- dplyr::select(desiguales,V32,V142,V128,V140)

#eliminar casos perdidos
base3$V32 <- recode(base3$V32, "c(88,99)=NA")
base3$V142 <- recode(base3$V142, "c(8,9)=NA")
base3$V128 <- recode(base3$V128, "c(88,99)=NA")
base3$V140 <- recode(base3$V140, "c(88,99)=NA")

#estadísticos descriptivos base 3
view(dfSummary(base3, headings = FALSE, method = "render"))

#correlación en base 3
tab_corr(base3)

#selección variables cuarto intento
base4 <- dplyr::select(desiguales,V2,V142,V128,V140)

#eliminar casos perdidos
base4$V142 <- recode(base4$V142, "c(8,9)=NA")
base4$V128 <- recode(base4$V128, "c(88,99)=NA")
base4$V140 <- recode(base4$V140, "c(88,99)=NA")

#estadísticos descriptivos base 4
view(dfSummary(base4, headings = FALSE, method = "render"))

#correlación en base 4
tab_corr(base4)

#selección variables quinto intento
base5 <- dplyr::select(desiguales,V15,V142,V128,V140)

#eliminar los casos perdidos
base5$V15 <- recode(base$V15, "c(88,99)=NA")
base5$V142 <- recode(base5$V142, "c(8,9)=NA")
base5$V128 <- recode(base5$V128, "c(88,99)=NA")
base5$V140 <- recode(base5$V140, "c(88,99)=NA")

#estadísticos descriptivos base 5
view(dfSummary(base5, headings = FALSE, method = "render"))

#correlación en base 5
tab_corr(base5)
