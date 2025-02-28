#Integrantes
#Ariel Aar�n Argomedo Madrid
#Mat�as Alejandro Barolo Tobar
#Ram�n Alejandro Parra Castillo


#Se importan las librer�as a utilizar
library(tidyverse)

#Se cargan los datos en un data frame
datos <- read.csv2("./EP10 Datos.csv", stringsAsFactors = FALSE, check.names = FALSE)


#Se filtra el data frame datos para tener los datos correspondientes a DisenoColor y LaKajita
DisenoColor <- filter(datos, Diseno == "DisenoColor")$Puntaje
LaKajita <- filter(datos, Diseno == "LaKajita")$Puntaje
LaKajita_Nino <- filter(datos, Diseno == "LaKajita" & Edad == "Nino" & Producto=="Cuchufli")$Puntaje
LaKajita_Joven <- filter(datos, Diseno == "LaKajita" & Edad == "Joven" & Producto=="Cuchufli")$Puntaje
LaKajita_Adulto <- filter(datos, Diseno == "LaKajita" & Edad == "Adulto" & Producto=="Cuchufli")$Puntaje

#Enunciado pregunta 1
#1. �Existe diferencia entre la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita?

#Para responder a esta pregunta se utilizar� una prueba de suma de rangos de Wilcoxon debido a que 
#que es la m�s adecuada debido a que no contamos con la condici�n de normalidad de los datos, haciendo uso de 
#esta prueba no param�trica y adem�s como se lograra comprobar m�s adelante cumple con las condiciones para 
#su uso.

#Se plantean las hip�tesis 
#H0: no hay diferencia en la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita (se distribuyen de igual forma).
#HA: s� hay diferencia en la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita (distribuciones distintas).

#Verificar condiciones
#1.- Las observaciones de ambas muestras son independientes. Los datos para el estudio fueron 
#    seleccionados aleatoriamente entre los participantes de un concurso.

#2.- La escala de medici�n debe ser a lo menos ordinal. Esto se logra cumplir en los datos debido a que
#    a que los productos se califican en una escala de Likert de 1 a 7. Siendo el valor de 1 el envase 
#    es muy poco atractivo y 7 el envase es muy atractivo.

#Mediante el siguiente proceso, se realiza la prueba de hip�tesis
#Se establece el nivel de significancia
alfa <- 0.05

#Se aplica la prueba de Mann-Whitney
prueba <- wilcox.test (DisenoColor , LaKajita , alternative = "two.sided", conf.level = 1 - alfa )
print ( prueba )

#Como el valor retornado por la prueba de Mann-Whitney tiene valor p = 0.0106 siendo menor a nuestro nivel de 
#significancia de 0.05, se puede decir con un 95% de confianza que rechaza la hip�tesis nula en favor de la hip�tesis
#alternativa, es decir, s� hay diferencia en la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita.


#Enunciado pregunta 2
# 2.- �Existe diferencias en las puntuaciones obtenidas para el envase de cuchufl� dise�ado por LaKajita seg�n la
#edad de los evaluadores? De ser as�, �cu�l(es) grupo(s) de evaluador(es) se diferencia(n) de los dem�s?

#Se aplica la Prueba de Kruskal-Wallis

#Planteamiento de hip�tesis
#H0: no hay diferencias significativas en las puntuaciones obtenidas para el envase de cuchufl� dise�ado por LaKajita seg�n la
#edad de los evaluadores.
#HA: hay diferencias en las puntuaciones obtenidas para el envase de cuchufl� dise�ado por LaKajita seg�n la
#edad de los evaluadores para al menos un grupo.

#Verificar condiciones
# 1.- La escala con que se mide la variable independiente es categ�rica con al menos dos niveles. Esta corresponde a la edad
# con los niveles Nino, Joven y Adulto.
# 2.- La escala de la variable dependiente es a lo menos ordinal. Esto si se cumple, dado que la variable dependiente puntaje
# corresponde a la escala de medici�n de Likert con valores entre 1 y 7.
# 3.- Los sujetos son una muestra aleatoria e independiente de la poblaci�n. Es correcto debido a que los individuos
# fueron escogidos de forma aleatoria en un concurso.

#Procedimiento y aplicaci�n de la prueba de Kruskal-Wallis.

Puntuacion <- c(LaKajita_Nino , LaKajita_Joven , LaKajita_Adulto)

Edad <- c( rep("LaKajita_Nino", length (LaKajita_Nino)), 
               rep ("LaKajita_Joven", length (LaKajita_Joven)), 
               rep ("LaKajita_Adulto", length (LaKajita_Adulto)))

Edad <- factor(Edad)

datosPruebaKruskal <- data.frame (Puntuacion , Edad)

#Se establece nivel de significaci�n
alfa <- 0.05

# Hacer la prueba de Kruskal - Wallis .
prueba <- kruskal.test ( Puntuacion ~ Edad , data = datosPruebaKruskal )
print ( prueba )

#Conclusion: Dado que el valor de p obtenido en la prueba es de 0.4003, al compararlo con el valor alfa de 0.05
#podemos concluir que no existe evidencia suficiente para rechazar la hip�tesis nula, por lo que, 
#no hay diferencias significativas en las puntuaciones obtenidas para el envase de cuchufl� dise�ado por LaKajita seg�n la
#edad de los evaluadores.
