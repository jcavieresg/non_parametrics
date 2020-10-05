#===========================================
#          Kolgomorov-Smirnov test
##===========================================


#_____________________________________________________________________________
# Ejemplo 1: Sumulación de una v.a Normal estandar
x = rnorm(100,0,1)
f = ecdf(x)
plot(f, xlab="Observaciones",main="ECDF",ylab="Probabilidad acumulada",col=4);
curve(pnorm(x,0,1),add=TRUE,col=2)


#====================================================
# Nota: Recuerde que la funcion pnorm calcula la cdf
#               F(x) = P(X <= x)

pnorm(2, mean=0,sd=1) # Numericamente
x = seq(-5,5,0.1)
hist = dnorm(x, mean=0, sd=1) 
plot(x, hist, type="l", xlab="Desviación estándar", ylab="Densidad",main="Plot de Densidad", lty = 1, lwd = 3, yaxs="i") # yaxs="i" localiza en el eje el límite del dato
polygon(c(x[x <= 2.0], 2), c(dnorm(x[x <= 2]), 0), col="lightblue")
text(0, .1,"0.977")


# Nota adicional
# ¿Usted cree que el tamaño de la muestra importa?

# ejemplo:
tot_poblacion = c(rep(1, 600), rep(0, 900))
tot_poblacion

tot_poblacion = sample(tot_poblacion)
tot_poblacion

#Media de la muestra en la poblacion
mean(tot_poblacion)

# Consideremos sólo 10 observaciones de la muestra
sample(tot_poblacion, 10)  # ==> ¿Cual sería su media?

mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))
mean(sample(tot_poblacion, 10))

# Cambiemos el tamaño de la muestra a 40
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))
mean(sample(tot_poblacion, 40))



# Cambiemos el tamaño de la muestra a 200
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))
mean(sample(tot_poblacion, 200))












#===============================================================================
                                    # Ejemplo 2
#===============================================================================


# Muestra aleatoria (H_0 verdadero)
n = 50
mu0 = 1
sd0 = 2
set.seed(12341)
samp = rnorm(n = n, mean = mu0, sd = sd0)

# Fn vs F0
plot(ecdf(samp), main = "", ylab = "Probabilidad")
curve(pnorm(x, mean = mu0, sd = sd0), add = TRUE, col = 2)


# Distancia máxima
# Ordenamos la muestra
samp_sorted <- sort(samp)
Ui <- pnorm(samp_sorted, mean = mu0, sd = sd0)
Dn_plus <- (1:n) / n - Ui
Dn_minus <- Ui - (1:n - 1) / n
i <- which.max(pmax(Dn_plus, Dn_minus))
lines(rep(samp_sorted[i], 2), 
      c(i / n, pnorm(samp_sorted[i], mean = mu0, sd = sd0)), 
      col = 4, lwd = 2)
rug(samp)
legend("topleft", lwd = 2, col = c(1:2, 4), 
       legend = latex2exp::TeX(c("$F_n$", "$F_0$", "sup_x|F_n(x)-F_0(x)|")))




                              #===============
                              # Ejemplo 2.1 
                              #===============

# Muestra desde una N(0, 1)
n = 50
set.seed(3245678) # dejamos una semilla para replicar resultados
x <- rnorm(n = n, mean = 0, sd = 1)

# Kolmogorov-Smirnov test for H_0: F = N(0, 1). 
(ks = ks.test(x = x, y = "pnorm")) # En "y" especificamos la cdf 

#__________________________________________________________________________
# No se rechaza H0 ya que si consideramos a un alpha 0.05, el p-value > alpha
#__________________________________________________________________________


# Vemos caracteristicas del obeto creado
str(ks)



                             #==============
                             # Ejemplo 2.1
                             #==============
# Kolmogorov-Smirnov test para H_0: F = N(0.5, 1)
ks.test(x = x, y = "pnorm", mean = 0.5)

#__________________________________________________________________________
# Rechazamos H0 ya que p-value < alpha (0.05)
#__________________________________________________________________________





                            #==============
                            # Ejemplo 2.2
                            #==============

# Kolmogorov-Smirnov test for H_0: F = Exp(2). 
ks.test(x = x, y = "pexp", rate = 1/2)

#__________________________________________________________________________
# Rechazamos H0 ya que p-value < alpha (0.05)
#__________________________________________________________________________








#====================================================
# Simulacion test K-S
#====================================================
# Tamaño muestra
n = 6

# Numero de simulaciones
m = 10000 


# Variable que inicializa
D=rep(0,m)

# Ciclo for para simular variables
for (j in 1:m){
# Simula variable Uniformes
x = runif(n,0,1)
# Ordenamos la muestra
x = sort(x)
# Calculo de la Fn(x)
Fn = ecdf(x)

#A la muestra ordenada le agregamos el 0 al principio para el caso F_n(x(0))=0
y = c(0,x)

# Busqueda del supremo
D1 = 0
D2 = 0
for (i in 2:(n+1)){
D1[i]=abs(Fn(y[i])-y[i])
D2[i]=abs(Fn(y[i-1])-y[i])
}

# Máximos
D[j]= max(D1,D2)
if( j %% 10000 == 0) print(j)
}


# Histograma que aproxima la distribucin K-S
hist(D, freq = FALSE, breaks = 15, main="Distribucion K-S")
lines(density(D))

round(quantile(D,c(0.80,0.90,0.95,0.98,0.99)),4)





