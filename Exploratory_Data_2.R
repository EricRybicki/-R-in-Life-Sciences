### Exploratory Data Analysis 2 for HarvardX Statistics and R for the life sciences.
# Scatter plots
library(UsingR)
data("father.son")
x=father.son$fheight
y=father.son$sheight
#Use scatter plot to show correlation between two paired data sets
plot(x,y,xlab="Father's height in inches",ylab="Son's height in inches",main=paste("correlation =",signif(cor(x,y),2)))
#Correlation defined as mean of the product of two standerdised data sets.
signif(cor(x,y),2)
#If the fathers height is 72, how would we predict the sons height?
#Stratify the sons' heights by the fathers heights, this creates groups of sons with who have a common height.
boxplot(split(y, round(x)))
print(mean(y[ round(x)==72]))
#Standerdise the units
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean)
fatherheights=as.numeric(names(means))
mypar2(1,1)
plot(fatherheights,means,ylab="average of strata of son heights",ylim=range(fatherheights))
abline(0,cor(x,y))
#Now plotting the standerdised means we can plot a slope and we find that the slope is indeed our correlation
#And so the five numbers and the mean are a very good summary for data that is in two dimensions
#and normally distributed.

#A caveat is that when we have some data that is uncorrelated but with an erronious data point
#much larger than the rest, we can find a false correlation. So it's worth visually inspecting for errors.
a=rnorm(100);a[1]=10
b=rnorm(100);b[1]=11
plot(a,b,main=paste("correlation =",signif(cor(a,b),2)))



###
install.packages("UsingR")
library(UsingR)
data(father.son)
plot(father.son$fheight, father.son$sheight)
cor(father.son$fheight, father.son$sheight)
########### Identify function allows you to click on points in the plot and after pressing ESC
# R identifies the row number of the points you clicked on
identify(father.son$fheight, father.son$sheight)
?identify

x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)
#The scale function subtracts the mean and divides by the standard deviation
plot(scale(x), scale(y))
abline(h=0, v=0)
mean(scale(x) * scale(y))
cor(x,y)
sum(scale(x) * scale(y)) / (n - 1)
#The correlation is nearly the same as multplying the scaled values but the pearson correlation
#uses n while the standard deviation uses n-1




### Log transformation to standerdize scale 
library(UsingR)
data(nym.2002)
head(nym.2002)
hist(nym.2002$time)
plot(nym.2002$age, nym.2002$time)
qqnorm(nym.2002$time)
barplot(tail(sort(table(nym.2002$home)),10))
boxplot(nym.2002$time ~ nym.2002$gender)

time = sort(nym.2002$time)
min(time)
min(time)/median(time)
max(time)/median(time)
#Plotting time over median time shows a skewed plot with a hard to grasp conclusion
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
#plotting the log2 of time over time more clearly demonstrates to us the distribution of times over med 
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)













