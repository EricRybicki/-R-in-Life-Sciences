### Matrix Algebra & Linear Models by HarvardX week 2
#Standard error in liner model
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)
A
9.8 
A %*% y 
-2 * (A %*% y) [3] 
A[3,3]


#Task: determine standard error in model over 100,000 simulations
tes <- replicate(100000,-2 * (A %*% (h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1))) [3])
std <- function(x) sd(x)/sqrt(length(x))
std(tes)
se <- function(x) sqrt(var(x)/length(x))
se(tes)
sd(tes) #The standard error of an estimate is the standard deviation of the sampling distribution of an estimate
#
betahat = replicate(B,{  
    y = 56.67 - 0.5*g*tt^2 + rnorm(n,sd=1)
    betahats = -2*A%*%y
    return(betahats[3])
})
#


library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef
#Return the SD of of the slope estimate for 10,000 simulations
betahat = replicate(10000, {
    index = sample(n,N)
    sampledat = father.son[index,]
    x = sampledat$fheight
    y = sampledat$sheight
    betahats = lm(y~x)$coef
    return(betahats[2])
})
sd(betahat)

#
B=10000
betahat = replicate(B,{
    index = sample(n,N)
    sampledat = father.son[index,]
    x = sampledat$fheight
    y = sampledat$sheight
    lm(y~x)$coef[2]  
})
sqrt ( mean( (betahat - mean(betahat) )^2 ))
#

#Coverience
mean( (Y - mean(Y))*(X-mean(X) ) )
mean((sampledat$fheight - mean(sampledat$fheight))*(sampledat$sheight-mean(sampledat$sheight)))



x =  father.son$fheight
beta =  c(34,0.5)
var(beta[1]+beta[2]*x)
y = h0 + v0*tt  - 0.5*g*tt^2 + rnorm(n,sd=1)
var(y)


###
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef
#var(betahat) = sigma2 (t(X) %*% X)^-1
#Sum of squared residuals
fit = lm(y ~ x)
fit$fitted.values
ssr<-sum((y-fit$fitted.values)^2)
sigma2 = ssr / 48
sigma2

X = cbind(rep(1,N), x)

#Standard error of the slope of beta hat
solve(t(X) %*% X)
sqrt(diag(solve(t(X) %*% X))*sigma2)

#
fit = lm(y ~ x)
sigma2 = sum((y - fit$fitted.values)^2) / (N - 2)
sqrt(sigma2 * diag(solve(t(X) %*% X)))
#
summary(fit)
#
##
###
##
#
x <- c(1,1,2,2)
f<-formula(~x)
?formula  
model.matrix(f)

group <- factor(c(1,1,2,2))
model.matrix(~ group)

group <- factor(c("control","control","treated","treated"))
model.matrix(~ group)

group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group)

group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group + 0)

group <- factor(c(1,1,1,1,2,2,2,2))
condition <- factor(c("a","a","b","b","a","a","b","b"))
model.matrix(~ group + condition)

model.matrix(~ group + condition + group:condition)
model.matrix(~ group*condition)

model.matrix(~ day + condition )#
model.matrix(~ condition )
~ day 
model.matrix(~ A + B + C + control + treated )
model.matrix(~ B + C + treated)
#
##
###
##
#
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
dat <- read.csv(filename)
stripchart(dat$Bodyweight ~ dat$Diet, vertical=TRUE, method="jitter",
           main="Bodyweight over Diet")

#Elaborating on data as factors/levels
levels(dat$Diet)
X <- model.matrix(~ Diet, data=dat)
X
colnames(X)
dat$Diet <- relevel(dat$Diet, ref="hf")
model.matrix(~ Diet, data=dat)
dat$Diet <- relevel(dat$Diet, ref="chow")

#Elaborating on what goes on within the lm function
Y <- dat$Bodyweight
X <- model.matrix(~ Diet, data=dat)
solve(t(X) %*% X) %*% t(X) %*% Y
s <- split(dat$Bodyweight, dat$Diet)
mean(s[["chow"]])
mean(s[["hf"]]) - mean(s[["chow"]])
fit <- lm(Bodyweight ~ Diet, data=dat)
summary(fit)
(coefs <- coef(fit))

#Viualize coefs. The distance from zero to control, and distance from control to the indicator variable
stripchart(dat$Bodyweight ~ dat$Diet, vertical=TRUE, method="jitter",
           main="Bodyweight over Diet", ylim=c(0,40), xlim=c(0,3))
a <- -0.25
lgth <- .1
library(RColorBrewer)
cols <- brewer.pal(3,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

#
##
###
##
#

nx=5
ny=7 
X = cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))
t(X) %*% X

