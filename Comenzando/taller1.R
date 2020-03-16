library(stringr)
#Para filtro se necesita esta librería:
library(dplyr)

graduados <- read.csv("https://www.datos.gov.co/api/views/ha9e-e48s/rows.csv?accessType=DOWNLOAD")
head(graduados)

#saco los nombres de los programas
carrera <- graduados$NOMBRE_PROGRAMA

#Saco los promedios acumulados de cada persona
promedio_graduados <- graduados$PACUMULADO/10

#Detecta caracteres y devuelve falso o verdadero si está la palabra o no
posgrados <- str_detect(carrera, "MAESTR.*|DOCTORADO.*|ESPECIALIZACION.*")
pregrado <- carrera[!posgrados]

notas_pregrado <- promedio_graduados[!posgrados]

facultades <- graduados$NOMBRE_FACULTAD
facultad_ingenieria <- str_detect(facultades, "INGENIERIA")

pregrado_ingenieria <- pregrado[facultad_ingenieria]
pregrado_ingenieria

nomPrograma <- graduados$NOMBRE_PROGRAMA

sexo <- graduados$SEXO

datos <- data.frame(facultades, nomPrograma, promedio_graduados, sexo)
View(datos)

fac_ing <- subset(datos, datos$facultades=="INGENIERIA")

head(fac_ing)

dim(fac_ing)

#verificamos los posgrados que hayan en la facultad de ingenieria y 
#se los concatenamos a la facultad de ingeniería
posg <- str_detect(fac_ing$nomPrograma, "MAESTR.*|DOCTORADO.*|ESPECIALIZACION.*")
fac_ing <- cbind(fac_ing, posg)

#sacamos a parte los graduados que no son de posgrados
fac_ing <- subset(fac_ing, fac_ing$posg == FALSE)

##Quitamos las columnas de banderas
fac_ing <- fac_ing[-5]



#Saco los ingenieros de sistemas con sus promedios categorizados
bool_sistemas <- str_detect(fac_ing$nomPrograma, "SISTEMAS")
ing_sistemas <- cbind(fac_ing, bool_sistemas)
ing_sistemas <- subset(ing_sistemas, ing_sistemas$bool_sistemas == TRUE)
ing_sistemas <- ing_sistemas[-5]
dim(ing_sistemas)
ing_sistemas$rendimiento[ing_sistemas$promedio_graduados < 3.5] <- "1.bajo"
ing_sistemas$rendimiento[ing_sistemas$promedio_graduados >= 3.5 & ing_sistemas$promedio_graduados < 4.0] <- "2.medio"
ing_sistemas$rendimiento[ing_sistemas$promedio_graduados >= 4.0 & ing_sistemas$promedio_graduados < 4.5] <- "3.alto"
ing_sistemas$rendimiento[ing_sistemas$promedio_graduados >= 4.5] <- "4.superior"

summary(ing_sistemas$promedio_graduados)

#Saco los ingenieros civiles
bool_civil <- str_detect(fac_ing$nomPrograma, "CIVIL")
ing_civil <- cbind(fac_ing, bool_civil)
ing_civil <- subset(ing_civil, ing_civil$bool_civil == TRUE)
ing_civil <- ing_civil[-5]

ing_civil$rendimiento[ing_civil$promedio_graduados < 3.5] <- "1.bajo"
ing_civil$rendimiento[ing_civil$promedio_graduados >= 3.5 & ing_civil$promedio_graduados < 4.0] <- "2.medio"
ing_civil$rendimiento[ing_civil$promedio_graduados >= 4.0 & ing_civil$promedio_graduados < 4.5] <- "3.alto"
ing_civil$rendimiento[ing_civil$promedio_graduados >= 4.5] <- "4.superior"


##Analisis para Ingeniería en sistemas
mujeres_sistemas <- subset(ing_sistemas, ing_sistemas$sexo == "F")
hombres_sistemas <- subset(ing_sistemas, ing_sistemas$sexo == "M")

#saco los sexos y el rendimiento para hacer la tabla de frecuencias
sexo_sistemas <- as.array(ing_sistemas$sexo)
rendimiento_sistemas <- as.array(ing_sistemas$rendimiento)

tabla_sistemas <- table(sexo_sistemas, rendimiento_sistemas)
tabla_sistemas
margin.table(tabla_sistemas)
addmargins(tabla_sistemas)
plot(sexo_sistemas, prom_sistemas)

prop_tabla_sistemas <- prop.table(tabla_sistemas, 1)
prop_tabla_sistemas
addmargins(prop_tabla_sistemas)
per_tabla <- prop_tabla_sistemas*100
per_tabla
addmargins(per_tabla)


hist(mujeres_sistemas$promedio_graduados, labels = T, ylim = c(0, 11), xlim = c(3.4, 4.1), main = "Histograma de mujeres en Ing sistemas", xlab = "Promedio")
plot(mujeres_sistemas$promedio_graduados, type = "h")


## Analisis para Ingeniería civil
mujeres_civil <- subset(ing_civil, ing_civil$sexo == "F")
hombres_civil <- subset(ing_civil, ing_civil$sexo == "M")

sexo_civil <- as.array(ing_civil$sexo)
summary(mujeres_civil$promedio_graduados)
summary(hombres_civil$promedio_graduados)
rendimiento_civil <- as.array(ing_civil$rendimiento)

tabla_civil <- table(sexo_civil, rendimiento_civil)
tabla_civil
addmargins(tabla_civil)
plot(sexo_civil, prom_civil)

prop_tabla_civil <- prop.table(tabla_civil, 1)
prop_tabla_civil
addmargins(prop_tabla_civil)
per_tabla_civil <- prop_tabla_civil*100
per_tabla_civil
addmargins(per_tabla_civil)

hist(mujeres_civil$promedio_graduados, labels = T, ylim = c(0, 11), xlim = c(3.4, 4.1))


#Histograma sexos de sistemas en porcentaje
grafica_mujer_sistemas <- barplot(per_tabla[1,], main = "Mujeres sistemas", ylim = c(0,100), ylab = "Porcentaje", xlab = "Rendimiento")
text(per_tabla[1,], pos=3, format(per_tabla[1,]),xpd = TRUE, col = "black")

grafica_hombre_sistema <- barplot(per_tabla[2,], main = "Hombres sistemas", ylim = c(0,100), ylab = "Porcentaje", xlab = "Rendimiento")
text(per_tabla[2,], pos=3, format(per_tabla[2,]),xpd = TRUE, col = "black")

#Histograma sexos de sistemas en porcentaje
grafica_mujer_civil <- barplot(per_tabla_civil[1,], main = "Mujeres civil", ylim = c(0,100), ylab = "Porcentaje", xlab = "Rendimiento")
text(per_tabla_civil[1,], pos=3, format(per_tabla_civil[1,]),xpd = TRUE, col = "black")


grafica_hombre_civil <- barplot(per_tabla_civil[2,], main = "Hombres sistemas", ylim = c(0,100), ylab = "Porcentaje", xlab = "Rendimiento")
text(per_tabla_civil[2,], pos=3, format(per_tabla_civil[2,]),xpd = TRUE, col = "black")


barplot(per_tabla, main="Hombres y Mujeres de Ing Sistemas",xlab="Rendimiento",ylab = "Porcentaje" , col=c("pink","blue"),legend = rownames(per_tabla), beside=TRUE, ylim = c(0,100))
barplot(per_tabla_civil, main="Hombres y Mujeres de Ing Civil",xlab="Rendimiento",ylab = "Porcentaje" , col=c("pink","blue"),legend = rownames(per_tabla_civil), beside=TRUE, ylim = c(0,100))

