#Integrantes:
# - Ariel Aaron Argomedo Madrid
# - Matias Alejandro Barolo Tobar
# - Ramon Alejandro Parra Castillo

#Importacion de librerias
library (tidyverse)
library(ggpubr)
library (ez)
#Se cargan los datos
data <- read.csv2('./EP08 Datos.csv',stringsAsFactors = FALSE, check.names = FALSE)

#Nivel de significancia
alfa <- 0.05

#Enunciado

# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan los usuarios
# en formular una consulta para un problema de dificultad media en las �reas de leyes, m�sica y matem�ticas.

#Se escogen los datos  a trabajar.

leyes <- filter(data,area=="Leyes" & dificultad=="Media")$tiempo
musica <- filter(data,area=="M�sica" & dificultad=="Media")$tiempo
matematicas <- filter(data,area=="Matem�ticas" & dificultad=="Media")$tiempo

# Se establecen las hip�tesis

#Lenguaje natural
# H0: El tiempo que tardan los usuarios en formular una consulta para un problema de dificultad media en las �reas de leyes
# m�sica y matem�ticas es igual.

# Ha: El tiempo que tardan los usuarios en formular una consulta para un problema de dificultad media en las �reas de leyes
# m�sica y matem�ticas es diferente para al menos un grupo.

#Lenguaje matem�tico
# H0: u1=u2=u3
# Ha: u1!=u2=u3 OR u1==u2!=u3 OR u1==u3!=u2

#Verificar condiciones:

# 1 - La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales. Esto es f�cilmente demostrable dado que al hablar de tiempos la diferencia entre por ejemplo los tiempos
# 10 y 5 es 5, al igual que la diferencia entre 15 y 10 que corresponde a 5.

# 2 - Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) poblaci�n(es) de origen. Se entiende que los
# datos recopilados fueron escogidos de forma aleatoria, por lo tanto, se cumple la condici�n de independencia.

# 3 - Se puede suponer razonablemente que la(s) poblaci�n(es) de origen sigue(n) una distribuci�n normal. Esta condici�n la podemos
# verificar haciendo uso de gr�ficos Q-Q.

datos <- data.frame(leyes,musica,matematicas)

datos <- datos %>% pivot_longer(c("leyes", "musica", "matematicas") ,
                                      names_to = "area",
                                      values_to = "tiempo")
datos [["area"]] <- factor ( datos [["area"]])
datos [["instancia"]] <- factor (1: nrow ( datos ) )

g <- ggqqplot ( datos ,
                x = "tiempo",
                y = "area",
                color = "area")
g <- g + facet_wrap (~area)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# Como podemos observar en los gr�ficos obtenidos, al no haber datos at�picos en ninguno de los gr�ficos Q-Q podemos decir
# que la condici�n de que la poblaci�n de origen siga una distribuci�n normal, se cumple.

# 4 - Las k muestras tienen varianzas aproximadamente iguales. 

# Para comprobar la cuarta condici�n se calculan las varianzas de cada grupo.

varLeyes <- var(leyes)
varMusica <- var(musica)
varMatematicas <- var(matematicas)

razonVarianzas <- max(varLeyes,varMusica,varMatematicas)/min(varLeyes,varMusica,varMatematicas)

cat("Raz�n entre maxima y minima varianza: ",razonVarianzas)

#Como podemos observar en el valor de la raz�n de las varianzas, este es menor a 1,5 por lo que se cumple la condici�n.

# Se aplica la prueba 

cat ("\nProcedimiento ANOVA usando aov\n\n")
prueba <- aov ( tiempo ~ area , data = datos )
print ( summary ( prueba ) )

# Luego de obtener los resultados de la prueba podemos observar que el valor p es igual a 1.16e-12 y si lo comparamos 
# con un nivel de significancia alfa igual a 0.05, podemos decir que se rechaza la hip�tesis nula en favor de la hip�tesis
# alternativa, es decir, podemos asegurar con un 95% de confiabilidad que el tiempo que tardan los usuarios en formular una 
# consulta para un problema de dificultad media en las �reas de leyes
# m�sica y matem�ticas es diferente para al menos un grupo.


# Si bien sabemos que existe una diferencia entre los tiempos que tardan los usuarios en formular una consulta para un problema
# de dificultad media en las �reas antes mencionadas, no sabemos espec�ficamente entre que grupos se da est� diferencia. Dicho lo anterior
# si deseamos obtener informaci�n acerca cu�les son los grupos que presentan diferencia se debe realizar un procedimiento Post-Hoc (Posteriori).


# Procedimiento Post-Hoc

post_hoc <- TukeyHSD (prueba,"area",ordered = TRUE,conf.level = 1 - alfa )

print (post_hoc)

# Luego de aplicar la prueba Post-Hoc, podemos observar que dado los valores p adj de los diferentes grupos obtenemos:
# matem�ticas-m�sica = 0.0000427
# leyes-m�sica = 0.0
# leyes-matem�ticas = 0.0043855

# Si comparamos los diferentes valores p adj con el nivel de significancia(alfa) obtenemos que en todos los casos 
# p adj < alfa, por lo que podemos decir finalmente que hay diferencia entre todos los diferentes grupos.

