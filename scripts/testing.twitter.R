library(data.table)
library(benford.analysis)
library(BenfordTests)

data.file <- fread(input = 'data/twitterAnonymized.csv', header = TRUE)
names(data.file) <- c('userID','friends','followers','tweets')

results <- data.frame(dataset = NA, measure = NA, chi = NA, ks = NA, f = NA, chd = NA, ed = NA, js = NA, jj = NA, jd = NA, mat = NA, mad = NA, df = NA)

for (i in 2:4) {
  d <- as.data.frame(data.file)[,i]
  
  t1 <- chisq.benftest(d)$p.value
  t2 <- ks.benftest(d)$p.value
  t3 <- usq.benftest(d)$p.value
  t4 <- mdist.benftest(d)$p.value
  t5 <- edist.benftest(d)$p.value
  t6 <- meandigit.benftest(d)$p.value
  t7 <- jpsq.benftest(d)$p.value
  t8 <- as.numeric(jointdigit.benftest(d)$p.value)
  
  benf.test <- benford(d, number.of.digits = 1)
  t9 <- benf.test$stats$mantissa.arc.test$p.value
  t10 <- benf.test$MAD
  t11 <- benf.test$distortion.factor

  results <- rbind(results, c('twitter', names(data.file)[i], t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11))
}

data.file <- fread(input = 'data/fbAnonymized.csv', header = TRUE)
names(data.file) <- c('userID','friends')

d <- as.data.frame(data.file)[,2]

t1 <- chisq.benftest(d)$p.value
t2 <- ks.benftest(d)$p.value
t3 <- usq.benftest(d)$p.value
t4 <- mdist.benftest(d)$p.value
t5 <- edist.benftest(d)$p.value
t6 <- meandigit.benftest(d)$p.value
t7 <- jpsq.benftest(d)$p.value
t8 <- as.numeric(jointdigit.benftest(d)$p.value)

benf.test <- benford(d, number.of.digits = 1)
t9 <- benf.test$stats$mantissa.arc.test$p.value
t10 <- benf.test$MAD
t11 <- benf.test$distortion.factor

results <- rbind(results, c('facebook', names(data.file)[2], t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11))

results <- results[-c(1),]

write.table(x = results, file = 'results.twitter.csv', row.names = FALSE)
