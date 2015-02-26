### Matrix Algebra & Linear Models by HarvardX week 2
#Calculation of linear models
m = matrix(c(1,1,1,1,0,0,1,1,0,1,0,1,0,0,0,1),4,4)
qr(m)$rank

sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))
X <- model.matrix( ~ sex + trt)
qr(X)$rank
Y <- 1:8

makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b

fitTheRest <- function(a,b) {
    Ystar <- makeYstar(a,b)
    Xrest <- X[,-c(2,5)]
    betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
    residuals <- Ystar - Xrest %*% betarest
    sum(residuals^2)
}

fitTheRest(1,2)

outer(1:3,1:3,`*`)
outer(-2:8,-2:8,Vectorize(fitTheRest))

library(rafalib)
imagemat(outer(-2:8,-2:8,Vectorize(fitTheRest)))


#
##
###
##
#
fit <- lm(friction ~ type + leg, data=spider)
betahat <- coef(fit)
Y <- matrix(spider$friction, ncol=1)
X <- model.matrix(~ type + leg, data=spider)
#R beta = Q^T Y

QR <- qr(X)
betahat <- solve.qr(QR, Y)


QR <- qr(X)
Q <- qr.Q( QR )
R <- qr.R( QR )
backsolve(R,crossprod(Q, Y))
betahat <- t(Q)%*%Y
qr(t(Q)%*%Y)

n <- 50;M <- 500
x <- seq(1,M,len=n)
X <- cbind(1,x,x^2,x^3)
beta <- matrix(c(1,1,1,1),4,1)
set.seed(1)
y <- X%*%beta+rnorm(n,sd=1)
