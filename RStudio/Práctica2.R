library(readr)
Data<- read_csv("Desktop/Probabilidad y Estadística/R/RStudio/PYE2DataSet92.csv")
View(PYE2DataSet92)

t.test(Data$sleeptime, conf.level = 0.9)
t.test(Data$sleeptime, conf.level = 0.95)
t.test(Data$sleeptime, conf.level = 0.99)

###########sleeptime########3
#Intervalo de confianza para 90:
#qnorm calcula el valor de z
CI_90<-qnorm(0.9)*var(Data$sleeptime)/100
intervalo_90<-c(mean(Data$sleeptime)-CI_90, mean(Data$sleeptime)+CI_90)

#95:
CI_95 <- qnorm(0.95)*var(Data$sleeptime)/100
intervalo_95<-c(mean(Data$sleeptime)-CI_95, mean(Data$sleeptime)+CI_95)

#99:
CI_99 <- qnorm(0.99)*var(Data$sleeptime)/100
intervalo_99<-c(mean(Data$sleeptime)-CI_99, mean(Data$sleeptime)+CI_99)


#####steps####
CI_90_steps<-qnorm(0.9)*var(Data$steps)/100
intervalo_90_steps <-c(mean(Data$steps)-CI_90_steps, mean(Data$steps)+CI_90_steps)

CI_95_steps<-qnorm(0.95)*var(Data$steps)/100
intervalo_95_steps <-c(mean(Data$steps)-CI_95_steps, mean(Data$steps)+CI_95_steps)








