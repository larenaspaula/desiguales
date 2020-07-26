#instalar paquetes necesarios para el trabajo

pacman::p_load(dplyr, #manipulacion de datos
               sjPlot, #tablas
               haven, #abrir base .sav
               car, #modificar base
               corrplot, #gráficos correlación
               summarytools, #estadisticos descriptivos
               fastDummies, # Crear variable dummy
               sjlabelled, #etiquetas variables
               ggplot2, #graficos
               coefplot # graficos de coeficientes
)

#abrir base de datos
desiguales <- read_spss("PNUD_DES_2016_publica.sav")

---#MODIFICACIÓN BASE DE DATOS

#selección variables quinto intento
base <- dplyr::select(desiguales,V15,V142,V128,V140)

#eliminar los casos perdidos
base$V15 <- recode(base$V15, "c(88,99)=NA")
base$V142 <- recode(base$V142, "c(8,9)=NA")
base$V128 <- recode(base$V128, "c(88,99)=NA")
base$V140 <- recode(base$V140, "c(88,99)=NA")

base <- filter(base, !is.na(V15))
base<- filter(base, !is.na(V142))
base <- filter(base, !is.na(V128))
base<- filter(base, !is.na(V140))

#ajustar nombres de las variables
base<- rename(base,desigualdad=V15,
              democracia=V142,
              economia=V128,
              politica=V140)

#estadísticos descriptivos 
view(dfSummary(base, headings = FALSE, method = "render"))


#correlación etre variables
tab_corr(base)

#regresión múltiple
desigualdad_democracia<-lm(desigualdad ~ democracia, data=base)
desigualdad_economia<-lm(desigualdad ~  economia, data=base)
desigualdad_politica<-lm(desigualdad ~  politica, data=base)
desigualdad_modelo <- lm(desigualdad ~ democracia + economia + politica, data = base)

#tabla de los distintos modelos de regresión
sjPlot::tab_model(list(desigualdad_democracia, desigualdad_economia,desigualdad_politica, desigualdad_modelo), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3","Modelo 4"),string.pred = "Predictores", string.est = "β")

#la variable mas influyente en los modelos de regresión
plot_model(desigualdad_modelo, show.values = TRUE)+ theme_sjplot()

#gráfico para visualizar las correlaciones

M <- cor(base)
corrplot(M, method = "circle") #aquí se decide el que se facilite más
corrplot(M, method = "number")

