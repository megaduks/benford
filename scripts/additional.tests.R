library(BenfordTests)
library(benford.analysis)
library(ggplot2)

library(dplyr)
library(tidyr)
library(reshape2)

x <- rnorm(1000, mean = 10)
y <- runif(1000, min = 1, max = 10)
z <- rbenf(1000)

chisq.benftest(x)
chisq.benftest(y)
chisq.benftest(z)

edist.benftest(x)
edist.benftest(y)
edist.benftest(z)

jointdigit.benftest(x)
jointdigit.benftest(y)
jointdigit.benftest(z)

jpsq.benftest(x)
jpsq.benftest(y)
jpsq.benftest(z)

ks.benftest(x)
ks.benftest(y)
ks.benftest(z)

mdist.benftest(x)
mdist.benftest(y)
mdist.benftest(z)

usq.benftest(x)
usq.benftest(y)
usq.benftest(z)

meandigit.benftest(x)
meandigit.benftest(y)
meandigit.benftest(z)

benford(x, number.of.digits = 1)
benford(y, number.of.digits = 1)
benford(z, number.of.digits = 1)

x <- 1:100

f1 <- 1/x
f2 <- 1/2^x
f3 <- 1/log(x+1)
f4 <- 1/factorial(x)
f5 <- 1/x^2
f6 <- 1/x^10

b <- log(1+1/x)

df <- data.frame(x,b,f1,f2,f3,f4,f5,f6)

ggplot(df, aes(x)) + 
  geom_line(aes(y= b, colour = 'Benford')) + 
  geom_line(aes(y=f1, colour = 'f1')) + 
  geom_line(aes(y=f2, colour = 'f2'))  + 
  geom_line(aes(y=f3, colour = 'f3')) + 
  geom_line(aes(y=f4, colour = 'f4')) + 
  geom_line(aes(y=f5, colour = 'f5')) + 
  geom_line(aes(y=f6, colour = 'f6')) + 
  scale_colour_brewer(palette = 'Set1')

chisq.benftest(f1)
edist.benftest(f1)
jointdigit.benftest(f1)
jpsq.benftest(f1)
ks.benftest(f1)
mdist.benftest(f1)
usq.benftest(f1)
meandigit.benftest(f1)
benford(f1, number.of.digits = 1)

chisq.benftest(f2)
edist.benftest(f2)
jointdigit.benftest(f2)
jpsq.benftest(f2)
ks.benftest(f2)
mdist.benftest(f2)
usq.benftest(f2)
meandigit.benftest(f2)
benford(f2, number.of.digits = 1)

chisq.benftest(f3)
edist.benftest(f3)
jointdigit.benftest(f3)
jpsq.benftest(f3)
ks.benftest(f3)
mdist.benftest(f3)
usq.benftest(f3)
meandigit.benftest(f3)
benford(f3, number.of.digits = 1)

chisq.benftest(f4)
edist.benftest(f4)
jointdigit.benftest(f4)
jpsq.benftest(f4)
ks.benftest(f4)
mdist.benftest(f4)
usq.benftest(f4)
meandigit.benftest(f4)
benford(f4, number.of.digits = 1)

chisq.benftest(f5)
edist.benftest(f5)
jointdigit.benftest(f5)
jpsq.benftest(f5)
ks.benftest(f5)
mdist.benftest(f5)
usq.benftest(f5)
meandigit.benftest(f5)
benford(f5, number.of.digits = 1)

chisq.benftest(f6)
edist.benftest(f6)
jointdigit.benftest(f6)
jpsq.benftest(f6)
ks.benftest(f6)
mdist.benftest(f6)
usq.benftest(f6)
meandigit.benftest(f6)
benford(f6, number.of.digits = 1)

uniform <- runif(10000, min = 1, max = 10)
log.uniform <- log10(u)
normal <- rnorm(10000, mean = 10)
log.normal <- log10(normal)

df <- data.frame(id = 1:1000, uniform, log.uniform, normal, log.normal)
tdf <- df %>%
  gather(distribution, value, -id)
ggplot(tdf, aes(x = value)) + geom_density() + facet_grid(. ~ distribution)

ggplot(tdf[tdf$distribution == 'log.normal',], aes(x = value)) + geom_density()
ggplot(tdf[tdf$distribution == 'normal',], aes(x = value)) + geom_density()
ggplot(tdf[tdf$distribution == 'uniform',], aes(x = value)) + geom_density()
ggplot(tdf[tdf$distribution == 'log.uniform',], aes(x = value)) + geom_density()
