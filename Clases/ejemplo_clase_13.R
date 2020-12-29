#==========================================
#           Ejemplo 1
#==========================================

"bootstrap"<- function(x,nboot,theta,...,func=NULL){
  call <- match.call()
  n <- length(x)
  bootsam<- matrix(sample(x,size=n*nboot,replace=TRUE),nrow=nboot)
  thetastar <- apply(bootsam,1,theta,...)
  func.thetastar <- NULL; jack.boot.val <- NULL; jack.boot.se <- NULL;
  if(!is.null(func)){
    match1 <- function(bootx,x){duplicated(c(bootx,x))[( length(x)+1) : (2*length(x))]}
    matchs <- t(apply(bootsam,1,match1,x))
    func.thetastar <- func(thetastar)
    jack.boot <- function(inout,thetastar,func){ func(thetastar[!inout])}
    jack.boot.val <- apply(matchs,2,jack.boot,thetastar,func)
    
    if(sum(is.na(jack.boot.val)>0)) {cat("At least one jackknife influence value for func(theta) is   undefined", fill=T)
      cat(" Increase nboot and try again",fill=T)
      return()}
    
    if( sum(is.na(jack.boot.val))==0) 
    {jack.boot.se <- sqrt( ((n-1)/n)*sum( (jack.boot.val-mean(jack.boot.val))^2 )  )
    
    
    
    }}
  
  return(list(thetastar=thetastar,func.thetastar=func.thetastar,jack.boot.val=jack.boot.val, jack.boot.se=jack.boot.se, call=call))

}




x = c(52, 104, 146,  10,  50,  31,  40,  27,  46)
z = c(94, 197,  16,  38,  99, 141,  23)


res = bootstrap(z, 50, mean)
res

hist(res$thetastar)
thetahat = mean(z)
abline(v=thetahat, lty=2)


res = bootstrap(z, 2000, mean)
res

hist(res$thetastar)
thetahat = mean(z)
abline(v=thetahat, lty=2)


# Error estandar de las 50 replicaciones
sd(res$thetastar)

# Haciendo 1000 replicaciones?
res2 = bootstrap(z, 1000, mean, func = sd)
res2


# Intervalo de confianza
conf.level = 0.95
probs = (1 + c(-1, 1) * conf.level) / 2
quantile(res$thetastar, probs = probs)




# Ejemplo Bootstrap
x = c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2,
      7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52, 6.21,
      12.28, 5.6, 5.38, 6.6, 8.74)


# Calculamos el coeficiente de variacion (CV) como el estadístico de interes
CV = function(x) sqrt(var(x))/mean(x)

# Calculamos el CV para nuestra muestra
CV(x)

# Una sola replicación mediante Bootstrap
sample(x, replace=T)

# calculamos el CV de la muestra
CV(sample(x,replace=T))


# Generamos 1000 muestras mediante Bootstrap. Primero creamos un vector de 0's:
boot = numeric(1000)
boot

# Ahora generamos 1000 muestras con nuestro CV en cada una de las 'celdas' de nuestro vector:
for (i in 1:1000) boot[i] <- CV(sample(x,replace=TRUE))

# Vemos la media y su varianza
mean(boot)
var(boot)

# Vemos su distribución mediante un simple histograma
hist(boot)

# El cuantil superior del 97.5%
quantile(boot,0.975)

# El cuantil inferior del 2.5%
quantile(boot,0.025)


# Siguiendo las notas del Bootsrap, el sesgo (Bias) es la diferencia entre la media de los
# valores del Bootstrap y el valor inicial estimado:
bias = mean(boot) - CV(x)

# y un Bootstrap corregido estimador del CV es:
CV(x) - bias


# Si asumimos Normalidad y aproximando un 95%, el intervalo de confianza sería:
# \hat{CV} +/- 1.96 \sqrt{Var(bootstrap)}
CV(x) - bias - 1.96*sqrt(var(boot)) # Inferior

CV(x) - bias + 1.96*sqrt(var(boot)) # Superior



#=================================================================
# Jackknife
length(x)
length(numeric(length(x)-1))

jack = numeric(length(x)-1)
jack

pseudo = numeric(length(x)) #pseudovalores (n-1 de la muestra de 'x')
pseudo


for (j in 1:length(x)) if(j < i) jack[j] <- x[j] else if(j > i) jack[j-1] <- x[j]

pseudo[i] = length(x)*CV(x) -(length(x)-1)*CV(jack)



jack <- numeric(length(x)-1)
pseudo <- numeric(length(x))
for (i in 1:length(x))
{ for (j in 1:length(x))
{if(j < i) jack[j] <- x[j] else if(j > i) jack[j-1] <- x[j]}
  pseudo[i] <- length(x)*CV(x) -(length(x)-1)*CV(jack)}


mean(pseudo)

var(pseudo)

hist(pseudo)

var(pseudo)/length(x)

mean(pseudo) + qt(0.975,length(x)-1)*sqrt(var(pseudo)/length(x))
mean(pseudo) - qt(0.975,length(x)-1)*sqrt(var(pseudo)/length(x))


# Otro ejemplo mediante Jackknife
library(bootstrap)

x = c(3.56, 0.69, 0.10, 1.84, 3.93, 1.25, 0.18, 1.13, 0.27, 0.50,
       0.67, 0.01, 0.61, 0.82, 1.70, 0.39, 0.11, 1.20, 1.21, 0.72)

x

# Media de la muestra
mean(x)

# Media mediante Jackknife (de la librería boostrap)
jackmean = jackknife(x, mean)
jackmean

# Bias-corregido jackknife estimador
meanjack = mean(x) - jackmean$jack.bias
meanjack

# Desvicion estandar de la muestra
sd(x)

# Desviacion estandar mediante Jackknife
jacksd = jackknife(x, sd)
jacksd

# Bias-corregido jackknife estimador
sdjack = sd(x) - jacksd$jack.bias
sdjack


# Varianza de la muestra
var(x)

# Varianza de la muestra mediante Jackknife 
jackvar = jackknife(x, var)
jackvar

# Bias-corregido jackknife estimador
varjack = var(x) - jackvar$jack.bias
varjack
