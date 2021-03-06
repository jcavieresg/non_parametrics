
#=======================================
#              Ejemplo 1
#=======================================
datos <- data.frame( sujeto = rep(1:15, each = 2),
                     tratamiento = c("pre" , "post" , "pre" , "post" , "pre" ,
                                     "post" , "pre" , "post" , "pre" , "post" ,
                                     "pre" , "post" , "pre" , "post" , "pre" ,
                                     "post" , "pre" , "post" , "pre" , "post" ,
                                     "pre" , "post" , "pre" , "post" , "pre" ,
                                     "post" , "pre" , "post" , "pre" , "post"),
                     respuesta = c("NO", "SI", "SI", "SI", "NO", "SI", "SI",
                                   "NO", "SI", "SI", "NO", "SI", "NO", "SI",
                                   "NO", "SI", "NO", "SI", "SI", "SI", "NO",
                                   "NO", "SI", "SI", "NO", "SI", "NO", "NO",
                                   "NO", "SI"))
head(datos)

#====================================================================================================================================
#                                                   Hip�tesis

# Ho: La respuesta de los sujetos es independiente del tratamiento. 
# (La proporci�n de sujetos que pasan de responder S� a No es igual a la proporci�n de sujetos que pasan de responder No a S�.)

# H1: El tratamiento s� influye en la respuesta 
# (por lo que la proporci�n de sujetos que pasan de No a S� es diferente de la de sujetos que pasan de S� a No.)
#====================================================================================================================================

# Se genera una tabla de contingencia con los datos

# En este caso se trata de una tabla de contingencia en la que cada sujeto se clasifica seg�n si 
# sus respuestas son iguales antes y despu�s del tratamiento y de c�mo han variado library(tidyverse)
library(tidyverse)
datos <- spread(data = datos, key = tratamiento, value = respuesta)
head(datos)

tabla <- table(Pre_Tratamiento = datos$pre, Post_Tratamiento = datos$post)
tabla

#================
# Test de McNemar

# La funci�n McNemar() tiene por defecto una correcci�n para mejorar la precisi�n del p-value cuando el n�mero de observaciones es peque�o. 
# Aun as� el test binomial es mejor en estos casos.
mcnemar.test(tabla)

binom.test(x = 1, n = 1 + 8, p = 0.5 )

# Conclusi�n

# Existen evidencias significativas para rechazar Ho en favor de que s� existe relaci�n entre el tratamiento y la respuesta de los sujetos.







#=======================================
#              Ejemplo 2
#=======================================
datos = matrix(c(794, 86, 150, 570),
                nrow = 2,
                dimnames = list("Antes" = c("S�", "No"),
                                "Desp�es" = c("S�", "No")))
datos


# Se trata de datos pareados, dos variables dicot�micas y el n�mero de observaciones que han cambiado su valor 
# en los dos niveles de la otra variable es >25. Se cumplen todas las condiciones para un test de McNemar.

#================
# Test de McNemar
mcnemar.test(datos)










