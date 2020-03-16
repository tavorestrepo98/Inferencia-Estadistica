library(dplyr)
library("Weighted.Desc.Stat")

x <- c(1, 2, 3, 4)
y <- c(0.1, 0.3, 0.4, 0.2)

#Distribucion probabilidad acumulada
plot(x, y, type = "h", col="blue")

#funcion de probabilidad discreta

#histograma de probabilidad

num_home_run <- c(0,1,2,3,4,5)
ocurrencias <- c(63, 51, 36, 9, 3, 0)
contador <- 0


for (variable in ocurrencias) {
  contador <- contador + variable
}

ocurrencias <- ocurrencias/contador
ocurrencias

plot(num_home_run, ocurrencias, type = "h", col="blue")

#Ejercicio de goles de juventus temporada 2018-2019
datos <- read.csv("https://www.football-data.co.uk/mmz4281/1819/I1.csv")
datos2 <- data.frame(datos$HomeTeam, datos$FTHG)
View(datos2)

datos_juventus <- subset(datos2, datos2$datos.HomeTeam=="Juventus")
View(datos_juventus)


tabla_juve <- table(datos_juventus$datos.FTHG)
p <- tabla_juve/sum(tabla_juve)

plot(row.names(tabla_juve), p, type = "h")

#Ejercicio de goles de juventus temporada 2019-2020
datos_2020 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/I1.csv")
datos2_2020 <- data.frame(datos_2020$HomeTeam, datos_2020$FTHG)
View(datos2_2020)

datos_2020_juventus <- subset(datos2_2020, datos2_2020$datos_2020.HomeTeam=="Juventus")
View(datos_2020_juventus)


tabla_2020_juve <- table(datos_2020_juventus$datos_2020.FTHG)
p_2020 <- tabla_2020_juve/sum(tabla_2020_juve)

p_2020
1-p_2020[1]

p
1-p[1]

plot(row.names(tabla_2020_juve), p_2020, type = "h")

#Esperanza E(x) es como el promedio sumatoria(X*f(x))
#ejercicio venta de vehículos esperanza y varianza

x2 <- c(0:5)
y2 <- c(0.18, 0.39, 0.24, 0.14, 0.04, 0.01)

weighted.mean(x2, y2)
#Esperanza y media son distintas
w.var(x2, y2)


#Ejercicio
x3 <- c(2, 4, 7, 8)
y3 <- c(0.2, 0.3, 0.4, 0.1)

weighted.mean(x3, y3)
w.var(x3, y3)
w.sd(x3, y3)


