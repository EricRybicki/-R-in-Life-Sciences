### Matrix Algebra & Linear Models by HarvardX week 2
#Complex Designs 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
boxplot(spider$friction ~ spider$type * spider$leg, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs ")
#Take a look at the friction of one spider type
spider.sub <- spider[spider$leg == "L1",]
fit <- lm(friction ~ type, data=spider.sub)
summary(fit)
(coefs <- coef(fit))
#Coefficients are just mean of the pull observations and the difference between the means of the two groups
s <- split(spider.sub$friction, spider.sub$type)
mean(s[["pull"]])
mean(s[["push"]]) - mean(s[["pull"]])
#Form the model matrix which was used by lm
X <- model.matrix(~ type, data=spider.sub)
colnames(X)
head(X)
tail(X)
#Visualize
stripchart(split(spider.sub$friction, spider.sub$type), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,3), ylim=c(0,2))
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
#A linear model with two variables
library(rafalib)
X <- model.matrix(~ type + leg, data=spider)
colnames(X)
head(X)
imagemat(X, main="Model matrix for linear model with two factors")

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)
(coefs <- coef(fitTL))


#Arrow plot to vis
spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols <- brewer.pal(5,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

install.packages("contrast")
library(contrast)
L3vsL2 <- contrast(fitTL,list(leg="L3",type="pull"),list(leg="L2",type="pull"))
L3vsL2


##
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
X<-model.matrix(~ species + condition)
X<-lm(species ~ condition)
contrast(X, list(species="B", condition="control"), list(species="A", condition="treatmed"))
#To use contrast a fit must be applied, the simple inclusion of rnorm(4) will allow a fit on the data
y = rnorm(4)
fit = lm(y ~ species + condition)
contrast(fit, list(species="B",condition="control"), list(species="A",condition="treated"))$X


L4vsL2 <- contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))
L4vsL2

fit <- lm(friction ~ type + leg, data=spider)
X <- model.matrix(~ type + leg, data=spider)
Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X)
Sigma


#
##
###Interactions and contrasts III
##
#
X <- model.matrix(~ type + leg + type:leg, data=spider)
colnames(X)
head(X)
imagemat(X, main="Model matrix for linear model with interactions")
fitX <- lm(friction ~ type + leg + type:leg, data=spider)
summary(fitX)
coefs <- coef(fitX)

#Draw arrow plot
stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols <- brewer.pal(8,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
# now the interactions:
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(4+a,coefs[1]+coefs[2]+coefs[3],4+a,coefs[1]+coefs[2]+coefs[3]+coefs[6],lwd=3,col=cols[6],length=lgth)
#
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(6+a,coefs[1]+coefs[4]+coefs[2],6+a,coefs[1]+coefs[4]+coefs[2]+coefs[7],lwd=3,col=cols[7],length=lgth)
#
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(8+a,coefs[1]+coefs[5]+coefs[2],8+a,coefs[1]+coefs[5]+coefs[2]+coefs[8],lwd=3,col=cols[8],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")
###
spider$log2friction <- log2(spider$friction)
boxplot(log2friction ~ type*leg, data=spider)
fit<-lm(log2friction ~ type*leg, data=spider)
summary(fit)
anova(fit)

L2vsL1 <- contrast(fit,list(leg="L2",type="push"),list(leg="L1",type="push"))
L2vsL1

###
N <- 40
p <- 4
group <- factor(rep(1:p,each=N/p))
X <- model.matrix(~ group)
tot <- data.frame()
i = 0
fin.ass <- replicate(1000, {
    i = i +1
    Y <- rnorm(N)
    mu0 <- mean(Y)
    initial.ss <- sum((Y - mu0)^2)
    s <- split(Y, group)
    after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
    group.ss <- initial.ss - after.group.ss
    group.ms <- group.ss / (p - 1)
    after.group.ms <- after.group.ss / (N - p)
    f.value <- group.ms / after.group.ms
})

mean(fin.ass)
hist(fin.ass, col="grey", border="white", breaks=50, freq=FALSE)
xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")


####
###
##
#
Fs = replicate(1000, {
    Y = rnorm(N,mean=42,7)
    mu0 = mean(Y)
    initial.ss = sum((Y - mu0)^2)
    s = split(Y, group)
    after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2)))
    (group.ss = initial.ss - after.group.ss)
    group.ms = group.ss / (p - 1)
    after.group.ms = after.group.ss / (N - p)
    f.value = group.ms / after.group.ms
    return(f.value)  
})
mean(Fs)