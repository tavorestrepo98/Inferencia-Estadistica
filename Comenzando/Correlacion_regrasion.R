gr<- c(50, 100, 200, 300)
peso <- c(50, 70, 70, 95)

prom_gr <- mean(gr)
s_gr <- sd(gr)
z_gr <- (gr-prom_gr)/s_gr

prom_peso <- mean(peso)
s_peso <- sd(peso)
z_peso <-(peso-prom_peso)/s_peso

r <- sum(z_gr*z_peso)/(length(z_gr)-1)

plot(gr, peso, type = "p")

#Graduados de universidad pedagógica
graduados <- read.csv("https://www.datos.gov.co/api/views/ha9e-e48s/rows.csv?accessType=DOWNLOAD")
head(graduados)

#saco los nombres de los programas
carrera <- graduados$NOMBRE_PROGRAMA

#Saco los promedios acumulados de cada persona
promedio_graduados <- graduados$PACUMULADO/10

summary(promedio_graduados)

levels(carrera)

library(stringr)
#Para filtro se necesita esta librería:
library(dplyr)

#Detecta caracteres y devuelve falso o verdadero si está la palabra o no
posgrados <- str_detect(carrera, "MAESTR.*|DOCTORADO.*|ESPECIALIZACION.*")
pregrado <- carrera[!posgrados]

notas_pregrado <- promedio_graduados[!posgrados]
length(notas_pregrado)

facultades <- graduados$NOMBRE_FACULTAD
facultad_ingenieria <- str_detect(facultades, "INGENIERIA")

pregrado_ingenieria <- pregrado[facultad_ingenieria]
pregrado_ingenieria
levels(pregrado_ingenieria)

nomPrograma <- graduados$NOMBRE_PROGRAMA



datos <- data.frame(facultades, nomPrograma, promedio_graduados)
head(datos)

fac_ing <- subset(datos, datos$facultades=="INGENIERIA")
head(fac_ing)

posg <- str_detect(fac_ing$nomPrograma, "MAESTR.*|DOCTORADO.*|ESPECIALIZACION.*")
fac_ing <- cbind(fac_ing, posg)
head(fac_ing)

fac_ing <- subset(fac_ing, fac_ing$posg == FALSE)
fac_ing <- fac_ing[-4]
head(fac_ing)

bool_sistemas <- str_detect(fac_ing$nomPrograma, "SISTEMAS")
bool_sistemas
ing_sistemas <- cbind(fac_ing, bool_sistemas)

ing_sistemas <- subset(ing_sistemas, ing_sistemas$bool_sistemas == TRUE)

bool_civil <- str_detect(fac_ing$nomPrograma, "CIVIL")
bool_civil
ing_civil <- cbind(fac_ing, bool_civil)

ing_civil <- subset(ing_civil, ing_civil$bool_civil == TRUE)



#Tarea: Datos tiene que tener el promedio de ingenieria de pregrado, Frecuencia X ingeniería
#Hacer tabla de contingencia






