#---Se instalan paquetes necesarios para la preparación de los datos y para los análisis

pacman::p_load(dplyr, #Manipulación de datos
               sjPlot, #Tablas
               sjlabelled, #Cambiar las etiquetas
               car, #Modificar base
               corrplot, #Gráficos correlación
               summarytools, #Estadísticos descriptivos
               webshot, #Exportar tablas
               gridExtra, #Unir gráficos
               ggplot2, #Gráficos
               lmtest, #Para test Breush-Pagan
               stargazer, #Tablas
               texreg, #Tablas
               coefplot # gráficos de coeficientes
               )


#---PREPARACIÓN DE DATOS

#Abrir base de datos (se realizó un proyecto en R, por lo que no se importa la base por SETWD)

PNUD_DES <- read.csv("PNUD_DES_2016_publica.csv")

#---MODIFICACIÓN BASE DE DATOS

#-Selección variables

desiguales <- dplyr::select(PNUD_DES,p9_6,p34,p27,p32,p14)

#-Eliminar los casos perdidos

#Se asignan los casos perdidos a sus respectivos valores

desiguales$p9_6 <- recode(desiguales$p9_6, "c(88,99)=NA")
desiguales$p34 <- recode(desiguales$p34, "c(8,9)=NA")
desiguales$p27 <- recode(desiguales$p27, "c(88,99)=NA")
desiguales$p32 <- recode(desiguales$p32, "c(88,99)=NA")
desiguales$p14 <- recode(desiguales$p14, "c(8,9)=NA")

#Se eliminan los casos perdidos de la base de datos

desiguales <- filter(desiguales, !is.na(p9_6))
desiguales<- filter(desiguales, !is.na(p34))
desiguales <- filter(desiguales, !is.na(p27))
desiguales<- filter(desiguales, !is.na(p32))
desiguales<- filter(desiguales, !is.na(p14))

#-Ajustar nombres de las variables

desiguales<- rename(desiguales,desigualdad=p9_6,
                    democracia=p34,
                    economia=p27,
                    politica=p32,
                    clase=p14)

#-Ajustar nombre de las etiquetas

#Visualizar etiquetas
get_label(desiguales) 

#Cambio en labels

desiguales$desigualdad <- set_label(x = desiguales$desigualdad,label = "Que tan desigual es Chile")
desiguales$democracia <- set_label(x = desiguales$democracia,label = "Funcionamiento de democracia")
desiguales$economia <- set_label(x = desiguales$economia,label = "Satisfaccion economica")
desiguales$politica <- set_label(x = desiguales$politica,label = "Satisfaccion politica")
desiguales$clase <- set_label(x = desiguales$clase,label = "En que clase se ubica")

#-Se guarda base de datos modificada

save(desiguales, file = "../desiguales/desigualescsv.RData")

#---ANÁLISIS DESCRIPTIVO

#---Análisis univariado  

#Estadísticos descriptivos 
view(dfSummary(desiguales, headings = FALSE, method = "render"), file = "descriptivoscsv.html")

#Se exporta la tabla
webshot("descriptivoscsv.html","descriptivoscsv.png")

#---Análisis bivariado

#-Correlación entre variables
tab_corr(desiguales, file="correlacioncsv.html")

#Se exporta la tabla
webshot("correlacioncsv.html","correlacioncsv.png", vwidth = 300,
        vheight = 300, zoom = 5)

#Gráfico para visualizar las correlaciones

M <- cor(desiguales)
corrplot(M, method = "circle") 
corrplot(M, method = "number")

# Gráficos de cajas

demog <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$democracia,
                     type = "box")

econog <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$economia,
                      type = "box")

politg <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$politica,
                      type = "box")

claseg <- plot_grpfrq(var.cnt = desiguales$desigualdad, var.grp = desiguales$clase,
                     type = "box")

# Unir gráficos
cajas<-grid.arrange(demog, econog, politg, claseg, nrow = 1)

#Se exporta el gráfico

ggsave("cajas.png",cajas,
       width = 46,
       height = 18,
       units = "cm")


#---ANÁLISIS MODELOS DE REGRESIÓN

#-Regresión múltiple 

#Regresión simple por cada variable  independiente y un modelo con todas las variables

desigualdad_democracia<-lm(desigualdad ~ democracia, data=desiguales)
desigualdad_economia<-lm(desigualdad ~  economia, data=desiguales)
desigualdad_politica<-lm(desigualdad ~  politica, data=desiguales)
desigualdad_clase<-lm(desigualdad ~ clase, data=desiguales)
desigualdad_modelo <- lm(desigualdad ~ democracia + economia + politica + clase, data = desiguales)

#Tabla de los distintos modelos de regresión
sjPlot::tab_model(list(desigualdad_democracia, desigualdad_economia,desigualdad_politica,desigualdad_clase, desigualdad_modelo), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3","Modelo 4", "Modelo 5"),string.pred = "Predictores", string.est = "β")

#La variable mas influyente en los modelos de regresión
plot_model(desigualdad_modelo, show.values = TRUE)+ theme_sjplot()

#-Tabla de regresión con SE

#Se crean los modelos de regresión

#Regresión simple
reg1<-lm(desigualdad ~ democracia, data=desiguales)
reg2<-lm(desigualdad ~  clase, data=desiguales)
reg3<-lm(desigualdad ~  politica, data=desiguales)
reg4<-lm(desigualdad ~ economia, data=desiguales)
#Regresión múltiple
reg5 <-lm(desigualdad ~ democracia + clase, data=desiguales)
reg6 <-lm(desigualdad ~ democracia + clase +economia, data=desiguales)
reg7<-lm(desigualdad ~ democracia + clase + economia + politica, data=desiguales)

#Se realiza la tabla

sjPlot::tab_model(list(reg1,reg2, reg3, reg4, reg5, reg6, reg7),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Modelo 7"),
                  string.pred = "Predictores",
                  string.est = "β", file="modelos de regresionSE.html")

#se exporta la tabla

webshot("modelos de regresionSE.html","modelos de regresionSE.png",
        vwidth = 300, vheight = 300, zoom = 5)


#---INFERENCIA ESTADÍSTICA

#-Se crean gráficos

in5<-sjPlot::plot_model(reg5,ci.lvl = c(0.95), title = "Regresión 5",vline.color = "grey",line.size = 1)
in6<-sjPlot::plot_model(reg6,ci.lvl = c(0.95), title = "Regresión 6",vline.color = "grey",line.size = 1)
in7<-sjPlot::plot_model(reg7,ci.lvl = c(0.95), title = "Regresión 7",vline.color = "grey",line.size = 1)

#Unir gráficos de inferencia

inferencia<-grid.arrange(in5, in6, in7, nrow = 1)

#Se exporta el gráfico 

ggsave("inferencia.png", inferencia,
       width =25 ,
       height =10 ,
       units = "cm")

#---DIAGNÓSTICOS

#-Casos influyentes

#Se establece punto de corte para descartar casos influyentes

n<- nobs(reg7) #n de observaciones
k<- length(coef(reg7)) # n de paramétros
dcook<- 4/(n-k-1) #punto de corte

#Se crea objeto "Final"
final <- broom::augment_columns(reg7,data = desiguales)
final$id <- as.numeric(row.names(final))

#Se filtran los casos influyentes

#Se identifican los casos
ident<- final %>% filter(.cooksd>dcook)

#Se crea otra base sin los casos influyentes
Desiguales <- final %>% filter(!(id %in% ident$id))

#Se genera el modelo sin los casos influyentes

reg8<- lm(desigualdad ~ democracia + clase + economia + politica,
          data=Desiguales)

#Se crean etiquetas para las tablas

labs <- c("Intercepto","Democracia","Clase",
            "Economía","Política")

#Se genera tabla comparativa entre regresión 7 y 8

htmlreg(list(reg7,reg8), 
        doctype = TRUE,
        custom.model.names = c("Modelo 7", "Modelo 8"),
        custom.coef.names = labs, file= "reg7y8.html")

#Se exporta la tabla

webshot("reg7y8.html","reg7y8.png", vwidth = 300,
        vheight = 300, zoom = 5)


#-Linealidad

#Se crea el gráfico de relación entre residuos y valores predichos

ggplot(reg8, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = TRUE)

#Se exporta el gráfico
ggsave("linealidad.png")

#-Test homogeneidad de varianza

#Prueba Chi Cuadrado
car::ncvTest(reg8)

#test Breush-Pagan
lmtest::bptest(reg8)

#Se crea el modelo de robustez
model_robust<- coeftest(reg8, vcov=vcovHC)

#Se genera tabla comparativa de regresión 7, 8 y modelo de robustez

htmlreg(list(reg7, reg8, model_robust), doctype = FALSE,
        custom.model.names = c("Modelo 7","Modelo 8", "M8 Robust"),
        custom.coef.names = labs, file= "robustez.html")

#Se exporta la tabla

webshot("robustez.html","robustez.png", vwidth = 300,
        vheight = 300, zoom = 5)


#-Multicolinealidad

#Se crean los objetos

vif1<-car::vif(reg7)
vif2<-car::vif(reg8)

#Se genera la tabla de multicolinealidad

stargazer::stargazer(list(vif1,vif2), type="latex", out= "vif.html")
 
#Se exporta la tabla

webshot("vif.html","vif.png", vwidth = 300,
        vheight = 300, zoom = 5)
                 
#---- BONUS TRACK

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
