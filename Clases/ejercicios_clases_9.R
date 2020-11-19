
#======================================================================
#                Problema para dos muestras independientes
# Location problem:  Hollander M, Wolfe D, Nonparametric statistical 
#                    methods, 2nd edition, Wiley 
#=====================================================================

# Mann-Whitney/Wilcoxon Test of location. Data is from Table 4.1.

x = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y = c(1.15, 0.88, 0.90, 0.74, 1.21)

mu = 0 # Hipotesis (no hay diferencia en las medianas)
m = length(x) # largo del vecvtor x
n = length(y) # largo del vector y
y = y - mu    # calculo de la media para y
data = c(x, y)# combinar ambos vectores
data

names(data) = c(rep("x", m), rep("y", n)) # Nombrar los datos
names(data)
data



data = sort(data) # Ordenamos los datos de menor a mayor
data

r = rank(data)    # Obtenemos los rangos
r

rbind(data, r)    # Combinamos todo en una matriz

w = sum(r[names(data) == "y"]) # Suma de rangos de Xilcoxon
w

u = w - n * (n + 1) / 2  # Mann-Whitney estadístico (Test U) 
u

pwilcox(u, m, n)  # Obtener p-value 


# Nota: Para una prueba de cola superior unilateral, la última línea se reemplazaría por
#                        1 - pwilcox(u - 1, nx, ny)

# Observación: Para una prueba de dos colas, haga la prueba de la cola inferior y 
# la prueba de la cola superior y duplique el valor P del menor de los dos resultados. 
# Esto se debe a que dos colas es dos veces una cola debido a la simetría de la distribución 
# nula del estadístico de prueba.


# Estimador de Hodges-Lehman (para \Delta) (a mano)
differences = sort(as.vector(outer(y, x, "-"))) 
DeltaHat = median(differences)
DeltaHat


# Estimación de Intervalos de confianza (a mano)
conf.level = 0.95
Ldiff = length(differences)
alpha = 1 - conf.level
w = qwilcox(alpha / 2, m, n)
if (w == 0) w = w + 1
cat("achieved confidence level:",
    1 - 2 * pwilcox(w - 1, m, n), "\n")
c(differences[w], differences[Ldiff + 1 - w]) # Intervalo de confianza



# Haciendo lo mismo anterior pero directamente en R
wilcox.test(y, x, alternative = "two.sided", exact = TRUE, correct = TRUE, 
            conf.int=TRUE, conf.level= 0.05)



#===============================
# Aproximación mediante n grande
wilcox.test(x, y, alternative = "less", exact = FALSE, correct = FALSE) 


# Ejemplo con rnorm para dos muestras independientes
wilcox.test(rnorm(10), rnorm(10, 2), conf.int = TRUE)



install.packages("DescTools")
library(DescTools)
# Para determinar los extremos!!
x = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y = c(1.15, 0.88, 0.90, 0.74, 1.21)

MosesTest(x, y)
MosesTest(y, x)


x = sample(1:20, 10, replace=TRUE)
y = sample(5:25, 6, replace=TRUE)

MosesTest(y, x)
