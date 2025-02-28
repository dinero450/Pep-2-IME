library (tidyverse)
library(ggpubr)
library (ez)

#En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan los usuarios
#en formular una consulta para un problema de dificultad media en las �reas de leyes, m�sica y matem�ticas.

datos <- read.csv2("./EP08 Datos.csv",stringsAsFactors = FALSE, check.names = FALSE)

datosLeyes <- filter(datos,area == "Leyes" & dificultad == "Media")
datosMusica <- filter(datos,area == "M�sica" & dificultad == "Media")
datosMatematicas <- filter(datos,area == "Matem�ticas" & dificultad == "Media")

datosAnova <- rbind(datosLeyes,datosMusica,datosMatematicas)
#Se coloca que hare y que pruebas hare y las condicones de anova

prueba2 <- ezANOVA(
  data = datosAnova,
  dv = tiempo,
  between = area,
  wid = id,
  return_aov = TRUE)

print(prueba2)
#pruebaAnova <- ezANOVA()