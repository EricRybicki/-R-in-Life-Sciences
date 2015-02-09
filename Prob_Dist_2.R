library(devtools)
install_github("jennybc/gapminder")

library(gapminder)
data(gapminder)
head(gapminder)
?gapminder

x <- gapminder$lifeExp[gapminder$year == 1952 ]
hist(x)
mean(x <= 40)

mean(x <=60 ) - mean(x <= 40)

plot(ecdf(x))
?ecdf

prop <- function(q) {
    mean(x <= q)
}

prop(40)

qs <- seq(from=min(x), to=max(x),length=20)
qs

props <- sapply(qs, prop)
props
plot(props)

props <- sapply(qs, function(q) mean(x <= q))
plot(props)
plot(ecdf(x))



pop <- gapminder$pop[gapminder$year == 1952 ]
hist(pop)
hist(log10(pop))
mean(log10(pop))
sd(log10(pop))

logpop <- log10(pop)
hist(logpop)
qqnorm(logpop)

z <- (logpop-mean(logpop))/sd(logpop)
head(z)
qqnorm(z)
abline(0,1)
tail(sort(z),1)

f <- function(q) pnorm(q, mean=mean(logpop), sd=sd(logpop))
n = length(logpop)
f(7)*n - f(6)*n
sum(logpop > 6 & logpop <= 7)

head(pnorm(logpop, mean=mean(logpop), sd=sd(logpop)))
mean(z)
sd(z)
head(pnorm(z))

qqnorm(logpop)
ps <- ((1:n) - 0.5)/n
qnorm(ps)
sort(logpop)
quantile(x <- qnorm(ps))
quantile(qnorm(ps), sort(logpop))

plot(qnorm(ps), sort(logpop))
qnorm(ps[1])
