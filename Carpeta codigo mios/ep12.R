library(tidyverse)
library(ggplot2)
library(ggpubr)
library ( rcompanion )
library(WRS2)

#Hipótesis en lenguaje natural
#H0: En promedio los algoritmos son igual de rápidos.
#Ha: En promedio los algoritmos no son igual de rápidos.

#Hipótesis en lenguaje matemático
#H0: u1-u2 = 0
#Ha: u1-u2 != 0

#1.- En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución (en
#milisegundos) medidos para dos versiones de un algoritmo genético para resolver instancias del problema
#del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro?

instancia_a <- c(129,109,28,178,74,16,87,108,149,78)
instancia_b <- c(134,193,10,88,142,86,36,190,163,33)
tiempo_a <- c(1510394,402929,885722,4428151,48667,834565,70599,783108,210041,37449)
tiempo_b <- c(1252837,2196277,120276,4629726,5743260,6701654,6568968,180141,6684497,35974)



datos <- data.frame(instancia_a,tiempo_a,instancia_b,tiempo_b)
datosCombinados <- c(tiempo_a,tiempo_b)
#Como se puede observar los datos para los tiempos del algoritmo A
#se distribuyen con una asimetría negativa.
g1 <- gghistogram ( datos , x = "tiempo_a", bins = 10,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = " blue ", fill = " blue ")

plot(g1)

#Como se puede observar los datos para los tiempos del algoritmo B
#se distribuyen de forma simétrica.
g2 <- gghistogram ( datos , x = "tiempo_b", bins = 10 ,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = "yellow", fill = "yellow")
plot(g2)


datosTransformados<- transformTukey (datosCombinados, start = -4 , end = 4 ,
                                     int = 0.0001 , returnLambda = FALSE)

datosTransformados2<- transformTukey (datos$tiempo_a, start = -4 , end = 4 ,
                                     int = 0.0001 , returnLambda = FALSE)


datosTransformados_a <- datosTransformados[1:10]
datosTransformados_b <- datosTransformados[11:20]

datosTransformadosCombinados <- data.frame(datosTransformados_a,datosTransformados_b)

g1 <- gghistogram ( datosTransformados2 , x = "datosTransformados2", bins = 10,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = " blue ", fill = " blue ")

plot(g1)

g2 <- gghistogram ( datosTransformadosCombinados , x = "datosTransformados_b", bins = 10 ,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = "yellow", fill = "yellow")
plot(g2)


cat("PREGUNTA 1\n")

# Cargar datos.
Tiempo.A <- c(337977, 303634, 33349, 243679, 3453176, 398653, 876432, 3073534,
              112326, 55026)

Tiempo.B <- c(335566, 52696, 3192222, 393213, 162808, 8765321, 76857, 231254,
              854213, 543215)

Algoritmo <- factor(c(rep("A", length(Tiempo.A)), rep("B", length(Tiempo.B))))
Tiempo <- c(Tiempo.A, Tiempo.B)
datos.1 <- data.frame(Algoritmo, Tiempo)

# Construimos el histograma de los datos.
g1 <- gghistogram(datos.1, x = "Tiempo", xlab = "Algoritmo", color = "Algoritmo",
                  fill = "Algoritmo", bins = 5)

g1 <- g1 + facet_grid(~ Algoritmo)
print(g1)

Log.tiempo <- log(datos.1[["Tiempo"]])
datos.1 <- cbind(datos.1, Log.tiempo)

g2 <- gghistogram(datos.1, x = "Log.tiempo", xlab = "Algoritmo", color = "Algoritmo",
                  fill = "Algoritmo", bins = 5)

g2 <- g2 + facet_grid(~ Algoritmo)
print(g2)


prueba.1 <- t.test(Log.tiempo ~ Algoritmo, data = datos.1)
cat("Prueba t de Student para log(Tiempo) de cada algoritmo:\n")
print(prueba.1)
