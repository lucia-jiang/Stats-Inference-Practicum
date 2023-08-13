#Inicializar datos
Data <- read_csv("Desktop/Probabilidad y Estadística/R/RStudio/PYE2DataSet92.csv")
set.seed(2021)

# Descriptiva:
summary(Data$sleeptime)
summary(Data$steps)

hist(Data$sleeptime)
hist(Data$steps)

boxplot(Data$sleeptime)
boxplot(Data$steps)

skewness(Data$sleeptime)
skewness(Data$steps)

kurtosis(Data$sleeptime)
kurtosis(Data$steps)

# Ajustes de sleeptime
library(MASS)
fitdistr(Data$sleeptime, c("normal"))
fitdistr(Data$sleeptime, c("gamma"))
fitdistr(Data$sleeptime, c("exponential"))

#superposición de curvas e histogramas
x=seq(0,1, 0.01) #definición del intervalo en donde representaremos la curva

hist(Data$sleeptime, freq = FALSE, ylim=c(0, 0.12), main="Histograma y curva de la dist.normal")
curve(dnorm(x, mean(Data$sleeptime), sd(Data$sleeptime)), col = "red", lwd = 3, add = TRUE)
hist(Data$sleeptime, freq = FALSE, ylim=c(0, 0.12), main="Histograma y curva de la dist.gamma")
curve(dgamma(x, 3.6607492, 0.4168438), col="blue", lwd=3, add=TRUE)
hist(Data$sleeptime, freq = FALSE, ylim=c(0, 0.12), main="Histograma y curva de la dist.exponencial")
curve(dexp(x, 0.113868421), col="blue", lwd=3, add=TRUE)


# Ajustes de steps
fitdistr(Data$steps, c("normal"))
fitdistr(Data$steps, c("gamma"))
fitdistr(Data$steps, c("exponential"))

#superposición de curvas e histogramas
hist(Data$steps, freq = FALSE, ylim=c(0, 7e-04), main="Histograma y dist.normal")
curve(dnorm(x, 11331.744737 ,1373.466347 ), col = "red", lwd = 3, add = TRUE)
hist(Data$steps, freq = FALSE, ylim=c(0, 7e-04), main="Histograma y dist.gamma")
curve(dgamma(x, 6.806330e+01, 6.006529e-03), col="blue", lwd=3, add=TRUE)
hist(Data$steps, freq = FALSE, ylim=c(0, 7e-04), main="Histograma y dist.exponencial")
curve(dexp(x, 8.824766e-05), col="green", lwd=3, add=TRUE)


# Test de kolmogorov
ks.test(Data$sleeptime,pnorm,8.78206610, 4.49717612)
ks.test(Data$sleeptime, pgamma, 3.660749175, 0.416843756)
ks.test(Data$sleeptime, "pexp", 0.113868421)

ks.test(Data$steps, "pnorm",11331.744737, 1373.466347)
ks.test(Data$steps, "pgamma", 6.806330e+01, 6.006529e-03)
ks.test(Data$steps, "pexp", 8.824766e-05)


#6. Gráfica
hist(Data$sleeptime, freq = FALSE, ylim=c(0, 0.12), main="Histograma y función de densidad")
dx <- density(Data$sleeptime)
lines(dx, lwd=2, col="blue")

hist(Data$steps, freq = FALSE, ylim=c(0, 7e-04), main="Histograma y función de densidad")
dx2<-density(Data$steps)
lines(dx2, lwd=2, col="green")

##########
#1.
set.seed(2021)
muestras30<-replicate(30, sample(Data$Age, 200))
#cambiar
media30<-colMeans(muestras30)
hist(media30)
boxplot(media30)
#Histograma en función de la densidad
hist(media30, freq=FALSE)
curve(dnorm(x, mean(media30), sd(media30)), col = "blue", lwd = 3, add = TRUE)

set.seed(2021)
muestras50<-replicate(50, sample(Data$Age, 200))
media50<-colMeans(muestras50)
hist(media50)
boxplot(media50)
#Histograma en función de la densidad
hist(media50, freq=FALSE)
curve(dnorm(x, mean(media50), sd(media50)), col = "blue", lwd = 3, add = TRUE)

set.seed(2021)
muestras100<-replicate(100, sample(Data$Age, 200))
media100<-colMeans(muestras100)
hist(media100)
boxplot(media100)
#Histograma en función de la densidad
hist(media100, freq=FALSE, ylim=c(0,1.5))
curve(dnorm(x, mean(media100), sd(media100)), col = "blue", lwd = 3, add = TRUE)

#2. Varianza
varianza30=1:30
for (i in 1:30) {varianza30[i]<-var(muestras30[1:200, i])} 
hist(varianza30)
boxplot(varianza30)
hist(varianza30, freq=FALSE, ylim=c(0,0.4))
curve(dnorm(x, mean(varianza30), sd(varianza30)), col = "yellow", lwd = 3, add = TRUE)


varianza50=1:50
for (i in 1:50) {varianza50[i]<-var(muestras50[1:200, i])} 
hist(varianza50)
boxplot(varianza50)
hist(varianza50, freq=FALSE)
curve(dnorm(x, mean(varianza30), sd(varianza30)), col = "yellow", lwd = 3, add = TRUE)


varianza100=1:100
for (i in 1:100) {varianza100[i]<-var(muestras100[1:200, i])} 
hist(varianza100)
boxplot(varianza100)
hist(varianza100, freq=FALSE)
curve(dnorm(x, mean(varianza100), sd(varianza100)), col = "yellow", lwd = 3, add = TRUE)


#Proporción de mujeres y varones

#Método para contar el número de varones
#Como se almacenan en putos vectores hacemos esto: :)
set.seed(2021)
muestras30sexo<-replicate(30,sample(Data$Sex, 200))
sexo30=1:30
for (i in 1:30){sexo30[i] <- sum(muestras30sexo[(1+200*(i-1)):(200*i)]=="V")}
proporcion30=1:30
for(i in 1:30){proporcion30[i]<-sexo30[i]/(200-sexo30[i])}
hist(proporcion30)
boxplot(proporcion30)
hist(proporcion30, freq=FALSE)
curve(dnorm(x, mean(proporcion30), sd(proporcion30)), col = "orange", lwd = 3, add = TRUE)



set.seed(2021)
muestras50sexo<-replicate(50,sample(Data$Sex, 200))
sexo50=1:50
for (i in 1:50){sexo50[i] <- sum(muestras50sexo[(1+200*(i-1)):(200*i)]=="V")}
proporcion50=1:50
for(i in 1:50){proporcion50[i]<-sexo50[i]/(200-sexo50[i])}
hist(proporcion50)
boxplot(proporcion50)
hist(proporcion50, freq=FALSE)
curve(dnorm(x, mean(proporcion50), sd(proporcion50)), col = "orange", lwd = 3, add = TRUE)


#muestra 100
set.seed(2021)
muestras100sexo<-replicate(100,sample(Data$Sex, 200))
sexo100=1:100
for (i in 1:100){sexo100[i] <- sum(muestras100sexo[(1+200*(i-1)):(200*i)]=="V")}
proporcion100=1:100
for(i in 1:100){proporcion100[i]<-sexo100[i]/(200-sexo100[i])}
hist(proporcion100)
boxplot(proporcion100)
hist(proporcion100, freq=FALSE,ylim=c(0,2.7))
curve(dnorm(x, mean(proporcion100), sd(proporcion100)), col = "orange", lwd = 3, add = TRUE)


#muestras de 200 de mujeres
set.seed(2021)
muestra_mujer_sleeptime <- sample(Data$sleeptime[Data$Sex=="M"],200)
mean(muestra_mujer_sleeptime)
var(muestra_mujer_sleeptime)

set.seed(2021)
muestra_mujer_steps <- sample(Data$steps[Data$Sex=="M"],200)
mean(muestra_mujer_steps)
var(muestra_mujer_steps)

set.seed(2021)
muestra_varon_sleeptime <- sample(Data$sleeptime[Data$Sex=="V"],200)
mean(muestra_varon_sleeptime)
var(muestra_varon_sleeptime)

set.seed(2021)
muestra_varon_steps <- sample(Data$steps[Data$Sex=="V"],200)
mean(muestra_varon_steps)
var(muestra_varon_steps)

