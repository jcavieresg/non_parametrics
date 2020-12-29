#==========================================
#           Ejemplo 1: Bootstrap
#==========================================

# 
x = rnorm(25,30,5)
B = 1000 # number of bootstrap samples to obtain
xbar = rep(0,B)
xbar # repetimos 'B' veces

# Creamos una funcion para muestrear con reemplazo
for( i in 1:B ) {
    xbs = sample(x,length(x),replace=TRUE)
    xbar[i] = mean(xbs)
}

xbar

# Desvest de xbar basada en la funcion anterior
se.xbar = sd(xbar)
se.xbar

# Como sabemos que la distribución de la media de la muestra se distribuye normalmente, 
# podemos calcular un intervalo de confianza aproximado del 95% utilizando valores t críticos de la siguiente manera
tcv= qt(0.975,length(x)-1)
mean(x)+c(-1,1)*tcv*se.xbar

# Intervalo t de la forma habitual de calculo
mean(x)+c(-1,1)*tcv*sd(x)/sqrt(length(x))


#===============================================
#             Ejercicio 2
#===============================================
# Con libreería boot
library(boot)
library(dplyr)
library(ggplot2)

x = runif(30)
hist(x)
mean(x)

resample = sample(x, replace = TRUE)
mean(resample)

# Muestrear 100 veces y encuentra la media de cada una
data_frame(num = 1:100) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(x, replace = TRUE))) %>% 
  ggplot(aes(x = means)) +
  geom_freqpoly()


# Muestrear 500 veces y encuentra la media de cada una
data_frame(num = 1:500) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(x, replace = TRUE))) %>% 
  ggplot(aes(x = means)) +
  geom_freqpoly()



# Crear una función para tomar una nueva muestra de los valores y luego calcular la media
boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector])
}

# R es el número de replicaciones
mean_results <- boot(x, boot_mean, R = 2000)

# Cargue la librería "broom" para obtener un marco de datos ordenado como salida.
library(broom)
tidy(mean_results)

# Calcular intervalos de confianza
boot.ci(mean_results)


#==================================================
#                Ejercicio 3
#==================================================
data = round(rnorm(100, 5, 3))
data[1:10] 

# Obtenemos 20 muestras bootstrap y 
# mostramos las primeras
resamples <- lapply(1:20, function(i) sample(data, replace = T))
resamples[5]

# Calculo de la media
r.median <- sapply(resamples, median)
r.median

# Calcular la desviación estándar de la distribución de medianas
sqrt(var(r.median))

# Histograma
hist(r.median)


# función boostrap para el error estándar de la mediana
b.median <- function(data, num) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)   
}

# generar los datos que se utilizarán (igual que en el ejemplo anterior)
data1 <- round(rnorm(100, 5, 3))

# saving the results of the function b.median in the object b1
b1 <- b.median(data1, 30)

# mostrando la primera de las 30 muestras de bootstrap
b1$resamples[1]

# mostrando el error estándar
b1$std.err

# displaying the histogram of the distribution of medians
hist(b1$medians)

# podemos ingresar los datos directamente en la función y mostrar el error estándar en una línea de código
b.median(rnorm(100, 5, 2), 50)$std.err

# mostrando el histograma de la distribución de medianas
hist(b.median(rnorm(100, 5, 2), 50)$medians)



#=============================================
#                Ejercicio 
#=============================================
# Creamos dos poblaciones
# Efron y Tibshirani

# Se quiere determinar si el consumo de dosis bajas de aspirina podía prevenir los ataques cardiacos en
# hombres sanos en edad media. La mitad de los participantes recibieron aspirina y la otra mitad un placebo.


#  grupo	      ataques cardiacos	  sujetos
# aspirina	      104	               11037
# placebo	        189	               11034


n <- c(11037, 11034)
s <- c( 119, 98)
# data for samples 1 and 2, where 1 = success (stroke), 0 = failure (no stroke)
dat1 <- c(rep(1, s[1]), rep(0, n[1] - s[1]))
dat2 <- c(rep(1, s[2]), rep(0, n[2] - s[2]))
# draw R bootstrap replicates
R <- 1000
# init location for bootstrap samples
bs1 <- rep(NA, R)
bs2 <- rep(NA, R)
# draw R bootstrap resamples of proportions
for (i in 1:R) {
# proportion of successes in bootstrap samples 1 and 2
# (as individual steps for group 1:)
resam1 <- sample(dat1, n[1], replace = TRUE)
success1 <- sum(resam1)
bs1[i] <- success1 / n[1]
# (as one line for group 2:)
bs2[i] <- sum(sample(dat2, n[2], replace = TRUE)) / n[2]
}

# bootstrap replicates of ratio estimates
rat <- bs1 / bs2
# sort the ratio estimates to obtain bootstrap CI
rat.sorted <- sort(rat)
# 0.025th and 0.975th quantile gives equal-tail bootstrap CI
CI.bs <- c(rat.sorted[round(0.025*R)], rat.sorted[round(0.975*R+1)])
CI.bs
## [1] 0.9399 1.5878




## Plot the bootstrap distribution with CI
# First put data in data.frame for ggplot()
dat.rat <- data.frame(rat)
library(ggplot2)
p <- ggplot(dat.rat, aes(x = rat))
p <- p + geom_histogram(aes(y=..density..)
                        , binwidth=0.02
                        , colour="black", fill="white")
# Overlay with transparent density plot
p <- p + geom_density(alpha=0.2, fill="#FF6666")
# vertical line at 1 and CI
p <- p + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
p <- p + geom_vline(aes(xintercept=CI.bs[1]), colour="#00AA00", linetype="longdash")
p <- p + geom_vline(aes(xintercept=CI.bs[2]), colour="#00AA00", linetype="longdash")
p <- p + labs(title = "Bootstrap distribution of relative risk ratio, strokes")
p <- p + xlab("ratio (red = 1, green = bootstrap CI)")
print(p)