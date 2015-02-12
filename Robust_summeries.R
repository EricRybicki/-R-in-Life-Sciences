### Robust summeries in HarvardX Statistics and R for the life sciences
#
data(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
head(ChickWeight)
#reshape the data from long to wide, where the columns Chick and Diet are the ID's
#and the column Time indicates different observations for each ID
?reshape
chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)
mean(c(chick$weight.4, 3000))/mean(chick$weight.4)
median(c(chick$weight.4, 3000))/median(chick$weight.4)
sd(c(chick$weight.4, 3000))/sd(chick$weight.4)
mad(c(chick$weight.4, 3000))/mad(chick$weight.4)
?mad

plot(chick$weight.4, chick$weight.21)
y<-cor(chick$weight.4, chick$weight.21)
x<-cor(c(chick$weight.4,3000), c(chick$weight.21,3000))
x/y


y<-cor(chick$weight.4, chick$weight.21, method = "spearman")
x<-cor(c(chick$weight.4,3000), c(chick$weight.21,3000), method = "spearman")
x/y



####
stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)
x <- chick$weight.4[chick$Diet == 1]
y <- chick$weight.4[chick$Diet == 4]
t.test(x,y)
wilcox.test(x,y)
x<-c(x,200)
t.test(x,y)
t.test(x,y)$p.value


x <- chick$weight.4[chick$Diet == 1]
y <- chick$weight.4[chick$Diet == 4]
par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y)$statistic
t.test(x,y+10)$statistic
t.test(x,y+100)$statistic

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

wilcox.test(c(1,2,3),c(4,5,6))
wilcox.test(c(1,2,3),c(400,500,600))
