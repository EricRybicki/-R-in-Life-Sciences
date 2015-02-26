#### Assesement 1.1
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- babies$bwt[babies$smoke==0]
pop.var <- var(bwt.nonsmoke)

n=50
var.test <- function(i){
    samp.var= var(sample(babies$bwt[babies$smoke==0], n))
    samp.var > pop.var*1.5
}
newtest <- replicate(1000,sapply(1:n, var.test))
mean(newtest)

### Solution
vars = replicate(1000, var(sample(bwt.nonsmoke, 50)))
hist(vars, breaks=100)
abline(v=pop.var, col="blue")
mean(vars > pop.var * 1.5)

### Further
sample.size <- 2:400
var.estimate <- sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")


#### Assessment 2
set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)

obs <- median(smokers) - median(nonsmokers)

avgdiff <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
    return(median(smokersstar) - median(nonsmokersstar))
})

mean(abs(avgdiff) > abs(obs))


















