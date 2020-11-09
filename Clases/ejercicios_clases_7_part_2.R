

#======================================================================
#                             Ejemplo 2
#======================================================================

alumnos_1  = c(14.8,10.6,7.3,12.5,5.6,12.9,6.3,16.1,9.0,11.4,4.2,2.7)
alumnos_2  = c(12.7,16.9,7.6,2.4,6.2,9.9,14.2,7.9,11.3,6.4,6.1,10.6,
               12.6,16.0,  8.3,9.1,15.3, 14.8,2.1,10.6,6.7,6.7,10.6,
               5.0,17.7,5.6,3.6,18.6,1.8,2.6,11.8, 5.6,1.0,3.2,5.9,4.0)


alumnos_1
alumnos_2


# A mano
m_Total = c(alumnos_1, alumnos_2)
rangos = rank(m_Total)
rangos
m_Total = cbind(m_Total, rangos)
m_Total


R1 = sum(rangos[1:12])
R2 = sum(rangos[12:48])
n1 = length(alumnos_1)
n2 = length(alumnos_2)
U1 = R1 - n1 * ( n1 + 1 ) / 2
U2 = R2 - n2 * ( n2 + 1 ) / 2
min(c( U1, U2 ))

wilcox.test(alumnos_1, alumnos_2, alternative="greater", paired=FALSE)

# Nota: 
# alternative	a character string that specifies the alternative hypothesis. Possible values are:
#              "two.sided"	to specify that the locations of x and y are different.
#              "greater"	to specify that the location of x is greater than that of y.
#              "less"	to specify that the location of x is less than that of y.


# paired	a logical value. If TRUE, the Wilcoxon signed rank test is computed. Default is the Wilcoxon rank sum test.


# Aproximacipón Normal (para muestras grandes y con empates)
library(coin)
grupo = factor(c(rep( "alumnos_1", length(alumnos_1)),
                 rep( "alumnos_2", length(alumnos_2))))
grupo
m_Total = c(alumnos_1, alumnos_2)
wilcox_test(m_Total ~ grupo, distribution = "exact" )






#======================================================================
#                             Ejemplo 3
#======================================================================

# Queremos determinar, basado en las siguientes muestas aleatorias, si existe
# diferencia entre las poblaciones de origen.

A = c(5, 5, 5, 5, 5, 5, 7, 8, 9, 10)
B = c(1, 2, 3 ,4, 5, 5, 5, 5, 5, 5)
par(mfrow = c(1,2))
hist(A, col = "cyan4", main = "")
hist(B, col = "firebrick", main = "")


# Como tenemos una muestra pequeña y las muestran tienen una clara asimetría, no podemos considerar
# el t-test para el contraste. Veamos con el test U

wilcox.test(A, B, paired = FALSE)

# El p-value es < 0.05, por tanto deberiamos rechazar H0, el cual sería que las poblaciones
# son las mismas. Sin embargo, si vemos las medianas de las muestras:
median(A)
median(B)

# Ya que las medianas son iguales, ¿como entonces debemos rechazar H0 basados en el p-value?

#====================================================================================
# Como las dos poblaciones tienen asimetrías en direcciones opuestas, es decir, 
# sus diferencias van más allá de la localización, el test de Mann-Whitney-Wilcoxon 
# no puede ser utilizado para comprar las medianas en este caso.
#=====================================================================================



# Conclusion
# El test de Mann-Whitney-Wilcoxon compara medianas, sin embargo, esto solo es cierto cuando las poblaciones 
# comparadas difieren únicamente es su localización, pero el resto de características (dispersión, asimetría.) 
# son iguales.

# Como ocurre en otros test no paramétricos, el test de Mann-Whitney-Wilcoxon (test U) es menos potente que el t-test 
# (tienen menos probabilidad de rechazar la H0 cuando realmente es falsa) ya que ignora observaciones en los extremos.

