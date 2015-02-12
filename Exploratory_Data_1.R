#Exploratory data analysis. HarvardX Statistics and R for the life sciences.

library(UsingR)
x=father.son$fheight
length(x)
# Take a sample of 20 values from 'x', and return rounded to one sig fig.
round(sample(x,20), 1)
# A range from min to max of 'x' assign limits and bin size
bins <- seq(floor(min(x)),ceiling(max(x)))
hist(x,breaks=bins,xlab="Height",main="Adult men heights")


## Empirical cummulative distribution function
#The for any value 'x', the ECDF reports back the proportion of values smaller or equal to 'x'.
myCDF <- ecdf(x) 
#We will evaluate the function at these values:
xs<-seq(floor(min(x)),ceiling(max(x)),0.1) 
# and then plot them:
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")


## Quantile qunatile plot
#When a distribution is normal, we find that the proportions seen in the last plot are roughly 
#equal to the normal approximation.
mean(x>70)
1-pnorm(70,mean(x),sd(x)) 
#population standard deviation ~~~~~ for reference
popsd <- function(x) sqrt(mean((x-mean(x))^2)) 
popsd(x)
1-pnorm(72,mean(x),popsd(x)) 
#Plot proportions against calculated normal
ps <- seq(0.01,0.99,0.01)
qs <- quantile(x,ps)
normalqs <- qnorm(ps,mean(x),popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line
## Pre-set R function for quantile-quantile plots
qqnorm(x)
qqline(x)
#We can confirm this works by ploting against known normally distributed data. 
n <-1000
x <- rnorm(n)
qqnorm(x)
qqline(x)
#For the opposite effect we can plot non-normally distributed data from t-distribution
#with different degrees of freedom. 
library(rafalib)
dfs <- c(3,6,12,30)
mypar2(2,2)
for(df in dfs){
    x <- rt(1000,df)
    qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
    qqline(x)
}


### We can visually inspect for a non-normal distribution of data by running a QQ plot on all columns of a data frame
load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
    qqnorm(dat[,i])
    qqline(dat[,i])
}
#Visually we see the plots of the fourth and ninth colums skew away from a normal distribution
par(mfrow = c(1,1))
hist(dat[,4])
hist(dat[,9])
#Histograms of said columns show a positive and negative distribution respectively. 


###When data is not normally distributed then the mean and standard deviation
#aren't necessarily good summaries. 
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)
#Heavily fat tailed distributions can better be described by a box plot
boxplot(exec.pay,ylab="10,000s of dollars",ylim=c(0,400))
fivenum(exec.pay)
mean(exec.pay)
#The box plot is a good visual of the five number distribution of a data frame. 


### We can also use the box plot to show a fivenum for multiple factors in a data frame.
head(InsectSprays)
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)
