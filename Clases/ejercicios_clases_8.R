#===========================================
#                 Ejemplo 1
#===========================================
# Generamos datos aleatorios
dat_1 = rpois(n=20, lambda=5)
dat_2 = rnorm(20)

ks.test(dat_1, dat_2)





#===========================================
#                 Ejemplo 2
#===========================================

# Test K-S (a manos)
m = c(71,72,74,76,77,78)
f = c(73,79,80,82,83,84)

m
f

Fm_Ff = rbind(cumsum(table(factor(m, levels=71:84)))/6,
        cumsum(table(factor(f, levels=71:84)))/6)

round(Fm_Ff,2)


plot(ecdf(m),col="blue",
     xlim=c(70,85),xlab="",ylab="",main="")
plot(ecdf(f),add=T,col="red",
     xlim=c(70,85),xlab="",ylab="",main="")  


D = abs(Fm_Ff[1,] - Fm_Ff[2,])

round(rbind(Fm_Ff,D),2)

max(D)


# Test K-S mediante función de R
ks.test(m, f)


