#Integrantes
#Ariel Aar�n Argomedo Madrid
#Mat�as Alejandro Barolo Tobar
#Ram�n Alejandro Parra Castillo

#Se cargan las librer�as a utilizar
library(tidyverse)
library(ggplot2)
library(ggpubr)
library ( rcompanion )
library(WRS2)

#Se cargan los datos a utilizar en un data frame
datosEP11 <- read.csv2("./EP11 Datos.csv", stringsAsFactors = FALSE, check.names = FALSE)

#Hip�tesis en lenguaje natural
#H0: En promedio los algoritmos son igual de r�pidos.
#Ha: En promedio los algoritmos no son igual de r�pidos.

#Hip�tesis en lenguaje matem�tico
#H0: u1-u2 = 0
#Ha: u1-u2 != 0

#1.- En el trabajo de t�tulo de un estudiante del DIINF se reportan los siguientes tiempos de ejecuci�n (en
#milisegundos) medidos para dos versiones de un algoritmo gen�tico para resolver instancias del problema
#del vendedor viajero disponibles en repositorios p�blicos. �Es uno de los algoritmos m�s r�pido que el otro?

instancia_a <- c(129,109,28,178,74,16,87,108,149,78)
instancia_b <- c(134,193,10,88,142,86,36,190,163,33)
tiempo_a <- c(1510394,402929,885722,4428151,48667,834565,70599,783108,210041,37449)
tiempo_b <- c(1252837,2196277,120276,4629726,5743260,6701654,6568968,180141,6684497,35974)

datos <- data.frame(instancia_a,tiempo_a,instancia_b,tiempo_b)
datosCombinados <- c(tiempo_a,tiempo_b)
#Como se puede observar los datos para los tiempos del algoritmo A
#se distribuyen con una asimetr�a negativa.
g1 <- gghistogram ( datos , x = "tiempo_a", bins = 10,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = " blue ", fill = " blue ")

plot(g1)

#Como se puede observar los datos para los tiempos del algoritmo B
#se distribuyen de forma sim�trica.
g2 <- gghistogram ( datos , x = "tiempo_b", bins = 10 ,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = "yellow", fill = "yellow")

plot(g2)

#Dado que estamos haciendo una transformaci�n a los datos de los tiempos del algoritmo A, debemos transformar tambi�n
#los datos de los tiempos del algoritmo B, dado que, para el primer algoritmo A se cambia la escala.

#Transformacion de los datos del algoritmo A utilizando Escalera de Potencias de Tukey.

datosTransformados<- transformTukey (datosCombinados, start = -4 , end = 4 ,
                                     int = 0.0001 , returnLambda = FALSE)

datosTransformados_a <- datosTransformados[1:10]
datosTransformados_b <- datosTransformados[11:20]

datosTransformadosCombinados <- data.frame(datosTransformados_a,datosTransformados_b)

#Una vez se ha realizado la transformaci�n a los datos, se procede a realizar la prueba t de student para dos
#muestras pareadas. Se considera t de student la m�s adecuada, ya que ambas muestras tienen un tama�o menor a 30.
#En primer lugar, debemos verificar las condiciones:

# Condiciones
# 1- Las observaciones son independientes entre s�, ya que son instancias distintas de la medici�n del problema.
# 2- Dado que anteriormente se realiz� una transformaci�n a los datos para que siguieran una distribuci�n aproximada a la normal
# se verifica la segunda condici�n.

# Procedimiento: Se realiza la prueba t de student para dos muestras pareadas, se considera la m�s adecuada ya que en ambas
# muestras el tama�o es menor a 30.
prueba <- t.test(x=datosTransformados_a,y=datosTransformados_b,paired=TRUE,alternative="two.sided", mu=0,sig.level=alfa)
print(prueba)

#Dado que, el valor p de la prueba t de student para dos muestras pareadas es mayor a alfa, entonces podemos concluir que
#se falla al rechazar la hip�tesis nula, esto quiere decir que con un 95% de confiabilidad no existe diferencia entre los tiempos
#de los algoritmos.

# 2- Analice la primera pregunta abordada en el ejercicio pr�ctico 11, con los mismos datos, utilizando un
# m�todo robusto adecuado.

B <- 1000
set.seed(666)

#Pregunta de investigaci�n EP11: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 a�os es igual.

#Hipotesis en lenguaje natural
#H0: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 a�os es igual.
#Ha: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 a�os es distinto.

#En teor�a, sabemos que se deber�a aplicar una prueba t de student para dos muestras independientes,
#sin embargo, como los datos son problem�ticos y no siguen una distribuci�n normal, se escoge la alternativa no param�trica.

datosHombre <- filter(datosEP11, sexo=="Hombre" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh
datosMujer <- filter(datosEP11, sexo=="Mujer" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh

#Comprobacion de normalidad
ggqqplot(datosHombre)
ggqqplot(datosMujer)

#De acuerdo a lo observado en los graficos qq si quisi�ramos aplicar la prueba de Yuen, tendr�amos que podar en un punto
#muy cercano a la mediana y dado que la cantidad de datos at�picos es enorme, estar�amos eliminando gran parte de los datos.
#Por lo que, utilizaremos como alternativa no param�trica la Prueab de Yuen con bootstrapping usando la mediana como medida de
#tendencia central.

ingresos <- c(datosHombre,datosMujer)
sexo <- c(rep("Hombre", length (datosHombre) ) , rep("Mujer", length (datosMujer) ) )

datosNormales <- data.frame(ingresos,sexo)

pruebaYuenMediana <-  pb2gen (ingresos ~ sexo, data = datosNormales , est = "median", nboot = B)

print(pruebaYuenMediana)

#Como el p valor igual a 0.002 es menor al nivel de significancia 0.05 se rechaza la hip�tesis nula en favor de la
#hipotesis alternativa. Podemos decir con un 95% de confiabilidad que si existe diferencia entre las medianas de los ingresos de hombres y mujeres
#solteros de entre 18 y 35 a�os.

# 3.- Analice la segunda pregunta abordada en el ejercicio pr�ctico 11, con los mismos datos, utilizando un
#     m�todo robusto adecuado 

#Se plantea la siguiente pregunta de investigaci�n: �La mediana del ingreso total del hogar corregido es igual para todos los niveles de educaci�n?

#Hipotesis en lenguaje natural
#H0: La mediana del ingreso total del hogar corregido es igual para todos los niveles de educaci�n
#Ha: La mediana del ingreso total del hogar corregido es distinta para todos los niveles de educaci�n.

set.seed(777)

datosp2 <- select(datosEP11,ytotcorh,educ)%>%filter(!is.na(ytotcorh) & !is.na(educ))

datosp2 <- sample_n(datosp2,400)

grupo_1 <- filter(datosp2, educ == "Sin Educ. Formal")$ytotcorh
grupo_2 <- filter(datosp2, educ == "B�sica Incom.")$ytotcorh
grupo_3 <- filter(datosp2, educ == "B�sica Compl.")$ytotcorh
grupo_4 <- filter(datosp2, educ == "M. Hum. Incompleta")$ytotcorh
grupo_5 <- filter(datosp2, educ == "M. T�c. Prof. Incompleta")$ytotcorh
grupo_6 <- filter(datosp2, educ == "M. Hum. Completa")$ytotcorh
grupo_7 <- filter(datosp2, educ == "M. T�c Completa")$ytotcorh
grupo_8 <- filter(datosp2, educ == "T�cnico Nivel Superior Incompleta")$ytotcorh
grupo_9 <- filter(datosp2, educ == "T�cnico Nivel Superior Completo")$ytotcorh
grupo_10 <- filter(datosp2, educ == "Profesional Incompleto")$ytotcorh
grupo_11 <- filter(datosp2, educ == "Postgrado Incompleto")$ytotcorh
grupo_12 <- filter(datosp2, educ == "Profesional Completo")$ytotcorh
grupo_13 <- filter(datosp2, educ == "Postgrado Completo")$ytotcorh
grupo_14 <- filter(datosp2, educ == "NS/NR")$ytotcorh

datosAnovaDependiente <- c(grupo_1,grupo_2,grupo_3,grupo_4,grupo_5,grupo_6,grupo_7,grupo_8,grupo_9,grupo_10,grupo_11,grupo_12,grupo_13,grupo_14)


datosAnovaIndependiente <- c(rep("Sin Educ. Formal", length (grupo_1) ) ,
                             rep("B�sica Incom.", length (grupo_2 ) ) ,
                             rep ("B�sica Compl.", length (grupo_3) ),
                             rep("M. Hum. Incompleta", length(grupo_4)),
                             rep("M. T�c. Prof. Incompleta" , length(grupo_5)),
                             rep("M. Hum. Completa", length(grupo_6)),
                             rep("M. T�c Completa", length(grupo_7)),
                             rep("T�cnico Nivel Superior Incompleta", length(grupo_8)),
                             rep("T�cnico Nivel Superior Completo", length(grupo_9)),
                             rep("Profesional Incompleto", length(grupo_10)),
                             rep("Postgrado Incompleto", length(grupo_11)),
                             rep("Profesional Completo", length(grupo_12)),
                             rep("Postgrado Completo", length(grupo_13)),
                             rep("NS/NR", length(grupo_14)))

dataPrueba <- data.frame(datosAnovaIndependiente,datosAnovaDependiente)
dataPrueba[["datosAnovaIndependiente"]] <- factor(dataPrueba[["datosAnovaIndependiente"]])

#Comprobaci�n de normalidad
data <- datosp2$ytotcorh
ggqqplot(data)

#Como se necesita una poda muy cercana a la mediana usaremos la prueba de ANOVA de una v�a para m�ltiples grupos
# independientes, sin embargo, como se logra observar en el gr�fico qq de los datos, estos no siguen una distribuci�n normal
# y si se quisiera aplicar una poda se tendr�a que descartar una gran cantidad de datos, es por esto, que se usa la mediana
# como medida de tendencia central.
#Debido a un problema presentado con la prueba med1way es que se utiliza la prueba Qanova para aplicar la prueba de hip�tesis a los datos.
#La prueba Qanova compara las medianas de cada grupo independiente, opcionalmente se utiliza par�metro q que corresponde a los cuartiles que se quiere
#comparar, sin embargo, cuando este valor no es ingresado toma por defecto 0.5 que corresponde a la mediana o al 50% de los datos.

pruebaAnova <- Qanova(datosAnovaDependiente ~ datosAnovaIndependiente, dataPrueba, nboot = 2000)
print(pruebaAnova)

#Conclusion
#Luego de realizar la prueba Qanova para ver si existe diferencia entre la mediana de los diferentes grupos independientes, el valor p obtenido es de 0.012, al
#ser comparado con un nivel de significancia 0.05 podemos concluir de que no existe evidencia suficiente para rechazar la hip�tesis nula. Esto quiere decir
#que podemos afirmar con un 95% de confianza que la mediana del ingreso total del hogar corregido es igual para todos los niveles de educaci�n.
