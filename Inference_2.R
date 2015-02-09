#Confidence intervals for a sample size of 30
dat <- read.csv("mice_pheno.csv")
head(dat)
pop <- dat[dat$Sex == "F" & dat$Diet == "chow", 3]
mu <- mean(pop)
mu

N <- 30
y <- sample(pop, N)
mean(y)

se <- sd(y)/sqrt(N)

Q <- qnorm(1-0.05/2)
##mean(y)-Q*se < mean(pop) < mean(y)+Q*se
interval <- c(mean(y)-Q*se,  mean(y)+Q*se)

plot(mu + c(-7,7), c(1,1), type = "n", xlab= "weights", ylab="intervals", ylim=c(1,100))
abline(v=mean(pop))
lines(interval, c(1,1))
for(i in 2:100){
    y <- sample(pop, N)
    se <- sd(y)/sqrt(N)
    interval <- c(mean(y)-Q*se,  mean(y)+Q*se)
    color <- ifelse(interval[1] <= mean(pop) & interval[2] >= mean(pop), 1, 2)
    lines(interval, c(i,i), col=color)
    #scan()
}



#Confidence intervals for sample size of 5
N <- 5
Q <- qt(1-0.05/2,4)   #Change from normal distribution to t-distribution
plot(mu + c(-7,7), c(1,1), type = "n", xlab= "weights", ylab="intervals", ylim=c(1,100))
abline(v=mean(pop))
for(i in 1:100){
    y <- sample(pop, N)
    se <- sd(y)/sqrt(N)
    interval <- c(mean(y)-Q*se,  mean(y)+Q*se)
    color <- ifelse(interval[1] <= mean(pop) & interval[2] >= mean(pop), 1, 2)
    lines(interval, c(i,i), col=color)
}



#Assessment 1
babies <- read.table("babies.txt", header=TRUE)
nonsmoke <- babies$bwt[babies$smoke == 0]
smoke <- babies$bwt[babies$smoke == 1]
N=30
Q <- qnorm(1-0.05/2)

testfun <-function(j){
    x <- sample(nonsmoke, N)
    y <- sample(smoke, N)
    mytest <- t.test(y,x)
    sum(mytest$conf.int)
    pro <- pro + (mean)
}

mean(replicate(1000, testfun(1000)))

## Solution
CIs <- replicate(1000, t.test(sample(nonsmoke, 30), sample(smoke, 30))$conf.int)
mean(CIs[2,] - CIs[1,])

popdiff <- mean(nonsmoke) - mean(smoke)
mean(CIs[2,]>popdiff & CIs[1,] < popdiff)



dat.ns = sample(nonsmoke, 30)
dat.s = sample(smoke, 30)
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/30 + sd.s^2/30)
tval = (X.ns - X.s)/sd.diff
qnorm(1-0.05/2)

ci.upper = (X.ns-X.s) + sd.diff*1.96
ci.lower = (X.ns-X.s) - sd.diff*1.96


#####
set.seed(1)
dat <- read.csv("mice_pheno.csv")
hfpop <- dat[dat$Sex == "F" & dat$Diet == "hf", 3]
chowpop <- dat[dat$Sex == "F" & dat$Diet == "chow", 3]

N <- 12
alpha <- 0.05
B <- 10000
rejections <- sapply(1:B, function(i){
    hf <- sample(hfpop, N)
    chow <- sample(chowpop, N)
    t.test(hf, chow)$p.value < alpha                               
})

head(rejections)
mean(rejections)

##
N <- 12
alpha <- 0.05
B <- 10000
Ns <- seq(5,50,5)
power <- sapply(Ns, function(N){
    rejections <- sapply(1:B, function(i){
        hf <- sample(hfpop, N)
        chow <- sample(chowpop, N)
        t.test(hf, chow)$p.value < alpha 
    })
    return(mean(rejections))
})
plot(Ns, power)


#######

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
N<-15
B <- 1000
alpha <- 0.01
power <- sapply(1:B, function(i){
    smoke <- sample(bwt.smoke, N)
    nsmoke <- sample(bwt.nonsmoke, N)
    t.test(smoke, nsmoke)$p.value < alpha
})
mean(power)
######


d <- read.csv("assoctest.csv")
table(d)
chisq.test(table(d))
fisher.test(table(d))
