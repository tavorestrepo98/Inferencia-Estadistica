5+5 

#Asignacion
y <- 8
x <- y + 3
nombre <- "Gustavo"

#Listar variales
ls()

#Eliminar variables
rm(y)

#Verificar tipo de dato: class()
class(nombre)

#Convercion de tipos
a <- 6
a <- as.character(a)

#Preguntar tipo
is.character(a)
is.double(a)

#arreglos
apellidos <- c("Restrepo", "Cardona", "Gil")
apellidos
class(apellidos)

###a partir de 1
apellidos[1]
apellidos[c(2, 3)]

  
#Comparacion
numeros <- c(1, 5, 7, 4, 65, 87, 12, 15, 8, 5)
numeros > 6
mayores_a6 <- numeros > 6
numeros[mayores_a6]
numeros+2
numeros


#Matrices  ##byrow=llenar por filas
m1 <- matrix(1:20, byrow = FALSE, nrow = 5, ncol =4)
m1

m2 <- matrix(1:20, byrow = FALSE, nrow = 4, ncol =4)
m2

m3 <- matrix(1:10, byrow = TRUE, nrow = 4, ncol =4)
m3

m1[1,3]
m1[1,]
m1[,2]

#Factores
estado_estudiante <- c("estudiante", "no estudiante", "estudiante", "no estudiante")
estado_estudiante <- factor(estado_estudiante)
levels(estado_estudiante)

mtcars
class(mtcars)
head(mtcars) #Primeros datos del dataframe
dim(mtcars)  #Dimensiones del dataframe
str(mtcars)  #Descripcion de cada variable

mtcars["Mazda RX4", ]
mtcars['mpg']

#Estadistica
lista <- 1:10
lista
mean(lista) #media
sd(lista)

