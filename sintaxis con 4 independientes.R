#instalar paquetes necesarios para el trabajo

pacman::p_load(dplyr, #manipulacion de datos
               sjPlot, #tablas
               sjlabelled, #cambiar las etiquetas
               haven, #abrir base.sav
               car, #modificar base
               corrplot, #gráficos correlación
               summarytools, #estadisticos descriptivos
               webshot, #guardar tablas
               gridExtra, #unir graficos
               coefplot # graficos de coeficientes
)

#abrir base de datos (se realizó un proyecto en R, por lo que  o se importa la base por SETWD)
PNUD_DES <- read_spss("PNUD_DES_2016_publica.sav")

#---MODIFICACIÓN BASE DE DATOS

#selección variables
desiguales <- dplyr::select(PNUD_DES,V15,V142,V128,V140,V70)

#eliminar los casos perdidos
desiguales$V15 <- recode(desiguales$V15, "c(88,99)=NA")
desiguales$V142 <- recode(desiguales$V142, "c(8,9)=NA")
desiguales$V128 <- recode(desiguales$V128, "c(88,99)=NA")
desiguales$V140 <- recode(desiguales$V140, "c(88,99)=NA")
desiguales$V70 <- recode(desiguales$V70, "c(8,9)=NA")

desiguales <- filter(desiguales, !is.na(V15))
desiguales<- filter(desiguales, !is.na(V142))
desiguales <- filter(desiguales, !is.na(V128))
desiguales<- filter(desiguales, !is.na(V140))
desiguales<- filter(desiguales, !is.na(V70))

#ajustar nombres de las variables
desiguales<- rename(desiguales,desigualdad=V15,
              democracia=V142,
              economia=V128,
              politica=V140,
              clase=V70)

#ajustar nombre de las etiquetas
get_label(desiguales) #visualizar etiquetas

desiguales$desigualdad <- set_label(x = desiguales$desigualdad,label = "Que tan desigual es Chile")
desiguales$democracia <- set_label(x = desiguales$democracia,label = "Funcionamiento de democracia")
desiguales$economia <- set_label(x = desiguales$economia,label = "Satisfaccion economica")
desiguales$politica <- set_label(x = desiguales$politica,label = "Satisfaccion politica")
desiguales$clase <- set_label(x = desiguales$clase,label = "En que clase se ubica")

#---ANÁLISIS DESCRIPTIVO

#---Análisis univariado  

#estadísticos descriptivos 
view(dfSummary(desiguales, headings = FALSE, method = "render"), file = "descriptivos.html")

#se exporta la tabla
webshot("descriptivos.html","descriptivos.png")

#nube de puntos
demog<- plot_scatter(data = desiguales,x = democracia,y = desigualdad,fit.grps = "lm")
econog<- plot_scatter(data = desiguales,x = economia,y = desigualdad,fit.grps = "lm")
politg<- plot_scatter(data = desiguales,x = politica,y = desigualdad,fit.grps = "lm")
claseg<- plot_scatter(data = desiguales,x = clase,y = desigualdad,fit.grps = "lm")

# Unir graficos
grid.arrange(demog, econog, politg, claseg, nrow = 1)

#---Análisis bivariado

#correlación etre variables
tab_corr(desiguales, file="correlacion.html")
#se exporta la tabla
webshot("correlacion.html","correlacion.png")

#gráfico para visualizar las correlaciones
M <- cor(desiguales)
corrplot(M, method = "circle") 
corrplot(M, method = "number")

#---ANÁLISIS MODELOS DE REGRESIÓN

#regresión múltiple 
desigualdad_democracia<-lm(desigualdad ~ democracia, data=desiguales)
desigualdad_economia<-lm(desigualdad ~  economia, data=desiguales)
desigualdad_politica<-lm(desigualdad ~  politica, data=desiguales)
desigualdad_clase<-lm(desigualdad ~ clase, data=desiguales)
desigualdad_modelo <- lm(desigualdad ~ democracia + economia + politica + clase, data = desiguales)

#tabla de los distintos modelos de regresión
sjPlot::tab_model(list(desigualdad_democracia, desigualdad_economia,desigualdad_politica,desigualdad_clase, desigualdad_modelo), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3","Modelo 4", "Modelo 5"),string.pred = "Predictores", string.est = "β")

#la variable mas influyente en los modelos de regresión
plot_model(desigualdad_modelo, show.values = TRUE)+ theme_sjplot()

#tabla de regresión con SE

#se hacen los modelos de regresión
reg1<-lm(desigualdad ~ democracia, data=desiguales)
reg2<-lm(desigualdad ~  clase, data=desiguales)
reg3<-lm(desigualdad ~  politica, data=desiguales)
reg4<-lm(desigualdad ~ economia, data=desiguales)
reg5 <-lm(desigualdad ~ democracia + clase, data=desiguales)
reg6 <-lm(desigualdad ~ democracia + clase +economia, data=desiguales)
reg7<-lm(desigualdad ~ democracia + clase + economia + politica, data=desiguales)

#se realiza la tabla
sjPlot::tab_model(list(reg1,reg2, reg3, reg4, reg5, reg6, reg7),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Modelo 7"),
                  string.pred = "Predictores",
                  string.est = "β", file="modelos de regresion.html")

#se exporta la tabla
webshot("modelos de regresion.html","modelos de regresion.png")


