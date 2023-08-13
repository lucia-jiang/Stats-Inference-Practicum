library(readr)
Data <- read_csv("Desktop/Probabilidad y Estadística/R/RStudio/PYE2DataSet92.csv")

set.seed(2021)
sample1<-sample(Data$IMC, 200)
sample2<-sample(Data$IMC, 200)

cuantil<-quantile(sample1)
t.test(sample1, alternative = "greater", mu=cuantil[2])
t.test(sample1, alternative="less", mu=cuantil[4])

sigma.test(sample1, sigmasq=1, alternative="greater")
t.test(sample1, sample2, paired=TRUE)

var.test(sample1, sample2)


# b)
sample<-sample1
ks.test(sample, pnorm, mean(sample), sd(sample))
pearson.test(sample) #xinru mona pls ayúdanos

set.seed(2021)
altura<-sample(Data$height, 200)
set.seed(2021)
peso<-sample(Data$weight, 200)
#preguntar a xinru <3

set.seed(2021)
peso<-sample(Data$weight, 200) 
mod1<-lm(sample ~ altura+peso)
dwtest(mod1)

wilcox.test(sample1, sample2)








