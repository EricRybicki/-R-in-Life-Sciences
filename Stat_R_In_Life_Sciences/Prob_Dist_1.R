dat <- read.csv("femaleMiceWeights.csv")
mean(dat[13:24,2]) - mean(dat[1:12,2])
s <- split(dat[,2], dat[,1])
stripchart(s, vertical = TRUE, col= 1:2)
abline(h=sapply(s, mean), col = 1:2)
highfat <- s[["hf"]] 
highfat
sample(highfat, 6)
?sample
sample(highfat, 6, replace = TRUE)
highfat > 30
as.numeric(highfat > 30)
sum(as.numeric(highfat > 30))
sum(highfat>30)
sum(highfat>30)/12
sum(highfat>30)/sum(highfat>0)

population <- read.csv("femaleControlsPopulation.csv")
control <- sample(population[,1], 12)
mean(control)

n <- 10000
null <- vector("numeric", n)
for (i in 1:n){
    control <- sample(population[,1], 12)
    treatment <- sample(population[,1], 12)
    null[i] <- mean(treatment) - mean(control)
}

diff <- mean(dat[13:24,2]) - mean(dat[1:12,2])
pv <- mean(null>diff)
pv

population <- population[,1]
mean(population)
sampleMean <- replicate(10000, mean(sample(population, 12)))
head(sampleMean)
plot(sampleMean)

null <- replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
head(null)
plot(null)
mean(null>diff)
hist(null)
abline(v=diff, col="red")
abline(v=-diff, col="red")

n <- 100
plot(0,0,xlim=c(-5,5),ylim=c(1,30),type="n")
totals <- vector("numeric",11)
for(i in 1:n){
    control <- sample(population,12)
    treatment <- sample(population,12)
    nulldiff <- mean(treatment) - mean(control)
    j <- pmax(pmin(round(nulldiff)+6,11),1)
    totals[j]<-totals[j]+1
    text(j-6,totals[j],pch=15,round(nulldiff,1))
    if(i < 15) scan() ##You can add this line to interactively see values appear
}




