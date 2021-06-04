
#====================================================
#           Ejemplo 1: test Kruskal-Wallis
#====================================================
data = data.frame(zonas = c(rep("zona1", 18), rep("zona2", 18), rep("zona3", 18)),
            n_paltas =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 
                          342, 40, 41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 
                          91,92, 93, 94, 293, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
                          25, 36, 37, 58, 59, 60, 71, 72))
head(data)

# Vemos la mediana para cada zona
aggregate(n_paltas ~ zonas, data = data, FUN = median)

# Vemos la mediana desviacion estandar de cada zona
aggregate(n_paltas ~ zonas, data = data, FUN = sd)


# Visualizaci?n
library(ggplot2)
ggplot(data = data, mapping = aes(x = zonas, y = n_paltas, colour = zonas)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")

ggplot(data = data, mapping = aes(x = n_paltas, colour = zonas)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ zonas) +
  theme(legend.position = "none")


# - El historgrama nos muestra que las muestras no se distribyen Normal, por tanto no podemos aplicar
# un test de ANOVA. 
# - Las tres muestras presentan asimetr?a hacia la derecha
# - El Test de Kruskal-Wallis es la opci?n m?s adecuada para este caso particular (sin comprobar por el
# momento homogeneidad de la varianza)
# - Otra alternativa ser?an las t?cnicas de resampling.



# Verificamos si existe homogneidad de la Varianza entre los grupos
library(car)
leveneTest(n_paltas ~ zonas, data = data, center = "median")

# No hay evidencias en contra de la homogeneidad de varianzas ya que Pr(>F) 0.458


# Nota: Test de Levene
# Se caracteriza, adem?s de por poder comparar 2 o m?s poblaciones, por permitir elegir entre diferentes 
# estad?sticos de centralidad :mediana (por defecto), media, media truncada. Esto es importante a la hora de contrastar 
# la homocedasticidad dependiendo de si los grupos se distribuyen de forma normal o no.


#========================
# Test de Kruskal-Wallis
#========================
kruskal.test(n_paltas ~ zonas, data = data)

# Se encuentra significancia en la diferencia de al menos 2 grupos.

# ? Cuales son los grupos difieren entre s?? 

#========================
#   An?lisis post-hoc
#========================
# M?todo de Holm por que es m?s flexible que el de Bonferroni
pairwise.wilcox.test(x = data$n_paltas, g = data$zonas, p.adjust.method = "holm" )



#====================================================
#           Ejemplo 2: test Kruskal-Wallis
#====================================================

library(NSM3)
cKW(0.0502, c(4, 4, 4), "Exact")

cKW(0.7147,c(20,20,20),"Monte Carlo",n.mc=20000)

### Datos

normal = c(2.9, 3.0, 2.5, 2.6, 3.2)   # normal subjects
oadisease = c(3.8, 2.7, 4.0, 2.4)           # subjects with obstructive airway disease
asbestosis = c(2.8, 3.4, 3.7, 2.2, 2.0)      # subjects with asbestosis

# Estamos interesados en utilizar Kruskal-Wallis el test de para probar si existen diferencias 
# en la mediana de los tiempos medios de eliminaci?n mucociliar para las tres poblaciones de sujetos. 
# Para este ejemplo consideramos el nivel de significancia es alfa = .0502.


# ara estos datos, tenemos n1 = n3 = 5, n2 = 4 y N = 14. 
# Combinando estos hechos con las sumas de rango de tratamiento calculamos:



kruskal.test(list(normal, oadisease, asbestosis))

#        Kruskal-Wallis rank sum test

#data:  list(normal, oadisease, asbestosis) 
#Kruskal-Wallis chi-squared = 0.7714, df = 2, p-value = 0.68





