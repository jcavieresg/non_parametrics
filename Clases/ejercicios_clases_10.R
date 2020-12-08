
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
#                                                   Hipótesis

# Ho: La respuesta de los sujetos es independiente del tratamiento. 
# (La proporción de sujetos que pasan de responder Sí a No es igual a la proporción de sujetos que pasan de responder No a Sí.)

# H1: El tratamiento sí influye en la respuesta 
# (por lo que la proporción de sujetos que pasan de No a Sí es diferente de la de sujetos que pasan de Sí a No.)
#====================================================================================================================================

# Se genera una tabla de contingencia con los datos

# En este caso se trata de una tabla de contingencia en la que cada sujeto se clasifica según si 
# sus respuestas son iguales antes y después del tratamiento y de cómo han variado library(tidyverse)
library(tidyverse)
datos <- spread(data = datos, key = tratamiento, value = respuesta)
head(datos)

tabla <- table(Pre_Tratamiento = datos$pre, Post_Tratamiento = datos$post)
tabla

#================
# Test de McNemar

# La función McNemar() tiene por defecto una corrección para mejorar la precisión del p-value cuando el número de observaciones es pequeño. 
# Aun así el test binomial es mejor en estos casos.
mcnemar.test(tabla)

binom.test(x = 1, n = 1 + 8, p = 0.5 )

# Conclusión

# Existen evidencias significativas para rechazar Ho en favor de que sí existe relación entre el tratamiento y la respuesta de los sujetos.







#=======================================
#              Ejemplo 2
#=======================================
datos = matrix(c(794, 86, 150, 570),
                nrow = 2,
                dimnames = list("Antes" = c("Sí", "No"),
                                "Despúes" = c("Sí", "No")))
datos


# Se trata de datos pareados, dos variables dicotómicas y el número de observaciones que han cambiado su valor 
# en los dos niveles de la otra variable es >25. Se cumplen todas las condiciones para un test de McNemar.

#================
# Test de McNemar
mcnemar.test(datos)










