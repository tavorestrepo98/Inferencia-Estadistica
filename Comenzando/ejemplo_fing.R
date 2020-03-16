fing <- read.csv("https://www.football-data.co.uk/mmz4281/1920/E0.csv")
head(fing)
dim(fing)

datos <- fing[,c(1:24)] #se guarda columnas del 1-24
head(datos)
#levels me sirve para mirar datos no repetidos
levels(datos$HomeTeam) # $ ->para acceder a las columnas

#media y desviacion de goles de locales
mean(datos$FTHG)
sd(datos$FTHG)

#media y desviacion de goles de visitante
mean(datos$FTAG)
sd(datos$FTAG)

#Caja de bigotes
#notas <- c(7.4, 7.9, 4.1, 8.1, 6.2, 7.1, 7.4, 6.7)
#mean(notas)
#sd(notas)
#median(notas)
class(datos$HomeTeam)
a_equipos <- as.array(datos$HomeTeam)
a_goles <- as.array(datos$FTHG)

#Crear dataframe
m <- data.frame(a_equipos, a_goles)
head(m)
m$a_goles


#tablas marginales
tab_local <- table(a_equipos, a_goles)
tab_local
plot(a_equipos, a_goles)
margin.table(tab_local)
tab_local[, 7]
addmargins(tab_local)

# proporcion 1 por filas, 2, por columnas
prop_locales <- prop.table(tab_local,1) #Probabilidad de que el local haga tantos goles
prop_locales

#Ajuste de porcentaje 
per_locales <- prop_locales*100
per_locales

rownames(per_locales)[1]
per_locales[1, ]

#Grafica de barras
grafica <- barplot(tab_local[1,], main = "Goles de Arsenal como local 2019-2020", ylim = c(0,10), ylab = "Partidos", xlab = "Goles" )
text(grafica, tab_local[1,], pos = 3, format(tab_local[1,]), xpd = TRUE, col = "blue") 


ars <- tab_local[1,]
ars
median(ars)
mean(ars)

#Encontrar la moda
tab_local
valores <- table(tab_local[1,])
valores

moda <- sort(valores, decreasing = TRUE)
moda
rownames(moda)[1]



### Analisis de frecuencia de tiros de esquina

a_esquina <- as.array(fing$HC)
a_esquina
head(fing2)

d_corners <- data.frame(a_equipos, a_esquina)
head(d_corners)

tab_esquina <- table(a_equipos, a_esquina)
tab_esquina
plot(a_equipos, a_esquina)
margin.table(tab_esquina)
 # tab_esquina[, 7
addmargins(tab_esquina)
prop_esquina <- prop.table(tab_esquina,1)
prop_esquina

per_esquina <- prop_esquina*100
per_esquina

grafica <- barplot(tab_esquina[1,], main = "corners del Arsenal como local 2019-2020", ylim = c(0,10), ylab = "Partidos", xlab = "corners" )
text(grafica, tab_esquina[1,], pos = 3, format(tab_esquina[1,]), xpd = TRUE, col = "blue")

ars_esquina <- tab_esquina[1,]
ars_esquina

median(ars_esquina)
mean(ars_esquina)

