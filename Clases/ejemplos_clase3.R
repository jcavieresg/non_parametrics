#=============================================
# EJEMPLOS: TEST chi-cuadrado (clase 3)
#=============================================

#============================
#         Ejemplo 1 
#============================

x = c(55, 50, 60, 65, 66, 60)

Tfit = chisq.test(x)

Tfit

round(Tfit$expected,digits=4)


round((Tfit$residuals)^2,digits=4)


#============================
#         Ejemplo 2
#============================

oc = c(6,57,206,362,365,256,69,13)
n = sum(oc)
range = 0:7
phat = sum(range*oc)/(n*7)
pmf = dbinom(range, 7,phat)

# Densidad de probabilidad 
rbind(range,round(pmf,3))

# Calculos del p-value
# Grados de libertad => k - 1 = 8 - 1 = 7 (pero ya tenemos estimado p así que es k - 1 - 1 = 6)
test.result = chisq.test(oc, p=pmf)
pchisq(test.result$statistic,df=6,lower.tail=FALSE)



#================================
# Ejemplo 3 test chi2 (a mano)
#================================

# Tenemos 3 partidos politicos (UDI, PS, PC)
# Simulamos votos para cada uno de ellos

votes = c(UDI=587, PS=552, PC=480)
votes

# Calcular las proporciones observadas
N = sum(votes)
N

votes_freq = votes/N
votes_freq

# Calcular las frecuencias esperadas (esperanza)
# Queremos determinar las frecuencias esperadas por cada clase. Tenemos 3 categorias y nuestra hipotesis nula es que
# los votantes estan igualmente distribuidos a través de las categorias, entonces H0 = 1/3.

#=============================================================================================================================
# Observación: Tenga en cuenta que también podríamos tener diferentes proporciones, por ejemplo, según los datos de otro 
# año (¡útil si quisiéramos determinar si los votantes de este año se están comportando de manera similar al año pasado!). 
# Solo necesitamos multiplicar nuestra proporción hipotética por nuestro tamaño de muestra, N, para calcular las frecuencias 
# esperadas.
#=============================================================================================================================

# Definimos H0
p = 1/3
p

# Calculamos la esperanza
E = N * p
E

# Nuestras frecuencias esperadas por cada clase debería ser N*p = 1619 * 1/3


# Ahora usando la aproximación hacia una chi-2
chisq_rs = (587 - 539.67)^2/539.67 + (552-539.67)^2/539.67 + (480 - 539.67)^2/539.67

# Calcular grados de libertad
# grados de libertad = k - 1, donde k numero de clases, entonces df = 3 - 1 = 2

# calcular p-value
p_val = pchisq(chisq_rs, df = 2, lower.tail = FALSE)
p_val



# Calculando con funciones de R
rs1 = chisq.test(votes)
rs1

# Por lo tanto (de los ejemplos a mano y función de R) 
# los votos son significativamente diferentes de lo que esperaríamos si un número igual de 
# votantes hubiera votado por cada categoría.

# Valos esperados en R
rs1$expected




#================================================
# Simulación exacta distribución T (ejemplo 4)
#================================================

# sim = numero de simulaciones
sim = 10000
#n = número de observaciones
n = 40
#k = número de clases
k = 3
#p = vector con las probabilidades de X_i este en cada una de las k clases
p = c(0.3,0.3,0.4)
# Simulacion de los observados
O = t(rmultinom(sim,n,p))
# Calculamos (observados - los esparados)^2/esperados
Tfit = (O-rep(1,sim)%*%t(n*p) )^2/(n*p)
#Sumamos
D=rowSums(Tfit)
#Obtenemos histograma
hist(D,freq=FALSE,breaks=30,main="Simulacion del Estadistico de Prueba")
#sobreponemos lo distribucion aproximada de chi.cuadrado
curve(dchisq(x,k-1),add=TRUE,col=2)


#comparacion de cuantiles
alpha = c(0.2,0.1,0.05,0.025,0.01)
a = quantile(D,1-alpha)
#cuantiles aproximados
b <- as.data.frame(round(t(qchisq(1-alpha,k-1)),2))
colnames(b) <- 1-alpha
a
