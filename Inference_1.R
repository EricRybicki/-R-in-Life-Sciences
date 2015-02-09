#### CLT demo from population
pop <- read.csv("mice_pheno.csv")
head(pop)

hf <- pop[pop$Diet == "hf" & pop$Sex == "F", 3]
chow <- pop[pop$Diet == "chow" & pop$Sex == "F", 3]
mean(hf) - mean(chow)

x <- sample(hf, 12)
y <- sample(chow, 12)
mean(x)-mean(y)

Ns <- c(3, 5, 10, 25)
B <- 10000
res <- sapply(Ns, function(n){
    sapply(1:B, function(j){
        mean(sample(hf,n)) - mean(sample(chow, n))
    })
})

library(rafalib)
mypar2(2,2)
for (i in seq(along=Ns)){
    title <- paste("Avg=", signif(mean(res[,i]),3))
    title <- paste(title, "SD=", signif(sd(res[,i]),3))
    qqnorm(res[,i], main = title)
    qqline(res[,i])
}


#### T.test from sample
dat <- read.csv("femaleMiceWeights.csv")
head(dat)

control <- dat[1:12, 2]
treatment <- dat[12+1:12, 2]
diff <- mean(treatment) - mean(control)

t.test(treatment,control)

sd(control)
sd(control)/sqrt(length(control))   ## Standard Error

se <- sqrt(var(treatment)/length(treatment) + var(control)/length(control))   ## Standard error of the difference

tstat <- diff/se

1-pnorm(tstat) + pnorm(-tstat)


qqnorm(control)
qqline(control)



#### Assesment
babies <- read.table("babies.txt", header=TRUE)
head(babies)
bwt.nonsmoke <- babies$bwt[babies$smoke == 0]
bwt.smoke <- babies$bwt[babies$smoke == 1]

mean(bwt.nonsmoke) - mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

N <- 30
dat.ns <- bwt.nonsmoke[1:30]
dat.s <- bwt.smoke[1:30]

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)
X.s <- mean(dat.s)
sd.s <- sd(dat.s)
sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.ns - X.s)/sd.diff

t.test(dat.ns, dat.s)$statistic

pval = 1-(pnorm(abs(tval))-pnorm(-abs(tval)))







