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

#abrir base de datos (se realizó un proyecto en R, por lo que no se importa la base por SETWD)

PNUD_DES <- read.csv("PNUD_DES_2016_publica.csv")

#---MODIFICACIÓN BASE DE DATOS

#selección variables
desiguales <- dplyr::select(PNUD_DES,p9_6,p34,p27,p32,p14)

#eliminar los casos perdidos
desiguales$p9_6 <- recode(desiguales$p9_6, "c(88,99)=NA")
desiguales$p34 <- recode(desiguales$p34, "c(8,9)=NA")
desiguales$p27 <- recode(desiguales$p27, "c(88,99)=NA")
desiguales$p32 <- recode(desiguales$p32, "c(88,99)=NA")
desiguales$p14 <- recode(desiguales$p14, "c(8,9)=NA")

desiguales <- filter(desiguales, !is.na(p9_6))
desiguales<- filter(desiguales, !is.na(p34))
desiguales <- filter(desiguales, !is.na(p27))
desiguales<- filter(desiguales, !is.na(p32))
desiguales<- filter(desiguales, !is.na(p14))

#ajustar nombres de las variables
desiguales<- rename(desiguales,desigualdad=p9_6,
                    democracia=p34,
                    economia=p27,
                    politica=p32,
                    clase=p14)

#ajustar nombre de las etiquetas
get_label(desiguales) #visualizar etiquetas

desiguales$desigualdad <- set_label(x = desiguales$desigualdad,label = "Que tan desigual es Chile")
desiguales$democracia <- set_label(x = desiguales$democracia,label = "Funcionamiento de democracia")
desiguales$economia <- set_label(x = desiguales$economia,label = "Satisfaccion economica")
desiguales$politica <- set_label(x = desiguales$politica,label = "Satisfaccion politica")
desiguales$clase <- set_label(x = desiguales$clase,label = "En que clase se ubica")

#se guarda base de datos modificada

save(desiguales, file = "../desiguales/desigualescsv.RData")

#---ANÁLISIS DESCRIPTIVO

#---Análisis univariado  

#estadísticos descriptivos 
view(dfSummary(desiguales, headings = FALSE, method = "render"), file = "descriptivoscsv.html")

#se exporta la tabla
webshot("descriptivoscsv.html","descriptivoscsv.png")

#---Análisis bivariado

#correlación etre variables
tab_corr(desiguales, file="correlacioncsv.html")

#se exporta la tabla
webshot("correlacioncsv.html","correlacioncsv.png", vwidth = 300,
        vheight = 300, zoom = 5)

#gráfico para visualizar las correlaciones
M <- cor(desiguales)
corrplot(M, method = "circle") 
corrplot(M, method = "number")

# Gráfico de cajas

demog <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$democracia,
                     type = "box")
econog <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$economia,
                      type = "box")
politg <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$politica,
                      type = "box")
claseg <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$clase,
                     type = "box")

# Unir gráficos
grid.arrange(demog, econog, politg, claseg, nrow = 1)

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
                  string.est = "β", file="modelos de regresion2.html")

#se exporta la tabla

webshot("modelos de regresion2.html","modelos de regresion2.png", vwidth = 300,
        vheight = 300, zoom = 5)

#regresión con clase

reg_1<-lm(clase ~ desigualdad, data=desiguales)
reg_2<-lm(clase ~ democracia , data=desiguales)
reg_3<-lm(clase ~  politica, data=desiguales)
reg_4<-lm(clase ~ economia, data=desiguales)
reg_5<-lm(clase ~ desigualdad + democracia + politica + economia , data=desiguales)

#se realiza la tabla de regresión
sjPlot::tab_model(list(reg_1,reg_2, reg_3, reg_4, reg_5),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
                  string.pred = "Predictores",
                  string.est = "β", file="modelos de regresion2.html")

#se exporta la tabla
webshot("modelos de regresion2.html","modelos de regresion2.png")

#---Inferencia estadística

