## HarvardX Matrix Algebra and Linear Models Week 1
library(UsingR)
?father.son

mean(father.son$sheight)
ref <- father.son$sheight[round(father.son$fheight) == 71]
mean(ref)
c(1,5,3,4)
rnorm(10)
X = matrix(1:1000,100,10)
X[25,3]
x=1:10
X<-cbind(x*1, x*2, x*3, x*4, x*5)
sum(X[7,])
matrix(1:60,20,3) 
matrix(1:60,20,3,byrow=TRUE) 
x=11:20;rbind(x,2*x,3*x) 
x=1:40;matrix(3*x,20,2)

t( t(X) ) 
X %*% matrix(1,ncol(X) )
X*1 
X%*%diag(ncol(X))

3a + 4b - 5c + d = 10
2a + 2b + 2c - d = 5
a -b + 5c - 5d = 7
5a + d = 4

X<-rbind(c(3,4,-5,1), c(2,2,2,-1), c(1,-1,5,-5), c(5,0,0,1))
Y <- matrix(c(10,5,7,4), 4,1)
solve(X) %*% Y
3*1.2477876 + 4*1.0176991 +5*0.8849558 -2.2389381

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
c<-a%*%b
c[3,2]
sum(a[3,]*b[,2])

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
X
beta <- c(5, 2)
X%*%beta

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)
X%*%beta
