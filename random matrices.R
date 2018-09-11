#Limpiamos la memoria
rm(list = ls())

#Generando las distribuciones para variables 
#aleatorias uniformes continuas en (0,1)
A1 = runif(100000,0,1)
par(mfrow=c(2,2))
for (i in 1:999){
  A1 = A1+runif(100000,0,1)
  if (i==1 | i==2 | i==99| i==999) {
    texto=paste0("Suma de ", i+1, " variables aleatorias")
    hist(A1, freq=FALSE, breaks=100,main=texto, col="lightblue", sub="Uniformes (0,1)", ylab="Densidad",xlab="Valores")
    }
}


#Generando las distribuciones para variables 
#aleatorias exponenciales con lambda=1
B1 = rexp(100000, rate = 1)
par(mfrow=c(2,2))
for (i in 1:999){
  B1 = B1+runif(100000,0,1)
  if (i==1 | i==2 | i==99| i==999) {
    texto=paste0("Suma de ", i+1, " variables aleatorias")
    hist(B1, freq=FALSE, breaks=100,main=texto, col="lightblue", sub="Exponenciales rate=1", ylab="Densidad",xlab="Valores")
  }
}

#Generando las distribuciones para variables 
#aleatorias normales estandar
C1 = rnorm(100000)
par(mfrow=c(2,2))
for (i in 1:999){
  C1 = C1+rnorm(100000)
  if (i==1 | i==2 | i==99| i==999) {
    texto=paste0("Suma de ", i+1, " variables aleatorias")
    hist(C1, freq=FALSE, breaks=100,main=texto, col="lightblue", sub="Normales estandar", ylab="Densidad",xlab="Valores")
  }
}

#Generando las distribuciones para variables 
#aleatorias Cauchy-Lorentz con localizacion=0 y escala 1
D1 = rcauchy(10000,location=0, scale=1)
par(mfrow=c(2,2))
for (i in 1:9999){
  D1 = D1+rcauchy(10000,location=0, scale=1)
  if (i==1 | i==2 | i==99| i==999) {
    texto=paste0("Suma de ", i+1, " variables aleatorias")
    #plot(density(D1))
    hist(D1, freq=FALSE, breaks=100,main=texto, col="lightblue", sub="Cauchy-Lorentz con escala=1", ylab="Densidad",xlab="Valores")
  }
}

