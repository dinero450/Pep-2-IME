library(tidyverse)


#�Existe diferencia entre la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita?
  
#se cargan los datos
datos <- read.csv2("./EP10 Datos.csv",stringsAsFactors = FALSE, check.names = FALSE)
lakajita <- filter(datos, Diseno == "LaKajita")$Puntaje
disenoColor <- filter(datos, Diseno == "DisenoColor")$Puntaje

alfa <- 0.05

#Es wilcoxon suma de rangos porque no se compara una diferencia entre lakajita sino entre dos dinsenos
# Pruebas de hipotesis:
# H0: no hay diferencia entre la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita
# HA: s� hay diferencia entre la puntuaci�n obtenida por los envases dise�ados por DisenoColor y LaKajita

prueba <- wilcox.test(lakajita , disenoColor , alternative = "two.sided", conf.level = 1 - alfa )
print ( prueba )


#Si hay diferencia


# �Existe diferencias en las puntuaciones obtenidas para el envase de cuchifl� dise�ado por LaKajita seg�n la
#edad de los evaluadores? De ser as�, �cu�l(es) grupo(s) de evaluador(es) se diferencia(n) de los dem�s?

nino <- filter(datos,Diseno == "LaKajita" & Edad == "Nino" & Producto == "Cuchufli")$Puntaje
joven <- filter(datos,Diseno == "LaKajita" & Edad == "Joven" & Producto == "Cuchufli")$Puntaje
adulto <- filter(datos,Diseno == "LaKajita" & Edad == "Adulto" &  Producto == "Cuchufli")$Puntaje


# Las hip�tesis a contrastar son, entonces:
# H0: No existe diferencias en las puntuaciones en todos los grupos de edades.
# HA: al menos uno de los grupos presenta un puntaje diferente a al menos alg�n otro grupo de edad.

puntajes <- c(nino,joven,adulto)

grupo <- c(rep("nino", length(nino)),
               rep("joven", length(joven)),
               rep("adulto", length(adulto)))

grupo <- factor(grupo)
datos1 <- data.frame(puntajes, grupo)

# Hacer la prueba de Kruskal-Wallis.
prueba <- kruskal.test(puntajes ~ grupo, data = datos1)
print(prueba)
  