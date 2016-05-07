library(data.table)
library(benford.analysis)
library(BenfordTests)
library(igraph)
library(dplyr)

data.file <- fread(input = 'data/livejournal.sample.csv', header = FALSE)
names(data.file) <- c('fromID', 'toID')

g <- graph.data.frame(data.file, directed = TRUE)

d <- igraph::degree(g, normalized = FALSE)
d <- d[!is.nan(d)]
b <- igraph::estimate_betweenness(g, directed = FALSE, nobigint = FALSE, cutoff = 10)
b <- b[!is.nan(b)]
c <- igraph::closeness(g, mode = 'all')
c <- c[!is.nan(c)]
l <- igraph::transitivity(g, type = 'local')
l <- l[!is.nan(l)]

#
# The dataframe containing all the results of conducted measurements. The columns are the following:
#   chi - Pearson's chi-square statistic
#   ks - Kolmogorov-Smirnov D test
#   f - Freedman's modification of Watson's U^2 statistic
#   chd - Chebyschev distance m statistic
#   ed - Euclidean distance d statistic
#   js - Judge-Schechter mean deviation a* statistic
#   jj - Joenssen's J^2P statistic
#   jd - Joing Digit Test T^2 statistic

results <- data.frame(dataset = NA, measure = NA, chi = NA, ks = NA, f = NA, chd = NA, ed = NA, js = NA, jj = NA, jd = NA, mat = NA, mad = NA, df = NA)

for (m in c('d', 'b', 'l', 'c')) {
  
  switch(m,
         d = { x = c(chisq.benftest(d)$p.value, ks.benftest(d)$p.value, usq.benftest(d)$p.value, mdist.benftest(d)$p.value, edist.benftest(d)$p.value, meandigit.benftest(d)$p.value, jpsq.benftest(d)$p.value, as.numeric(jointdigit.benftest(d)$p.value))},
         b = { x = c(chisq.benftest(b)$p.value, ks.benftest(b)$p.value, usq.benftest(b)$p.value, mdist.benftest(b)$p.value, edist.benftest(b)$p.value, meandigit.benftest(b)$p.value, jpsq.benftest(b)$p.value, as.numeric(jointdigit.benftest(b)$p.value))},
         l = { x = c(chisq.benftest(l)$p.value, ks.benftest(l)$p.value, usq.benftest(l)$p.value, mdist.benftest(l)$p.value, edist.benftest(l)$p.value, meandigit.benftest(l)$p.value, jpsq.benftest(l)$p.value, as.numeric(jointdigit.benftest(l)$p.value))},
         c = { x = c(chisq.benftest(c)$p.value, ks.benftest(c)$p.value, usq.benftest(c)$p.value, mdist.benftest(c)$p.value, edist.benftest(c)$p.value, meandigit.benftest(c)$p.value, jpsq.benftest(c)$p.value, as.numeric(jointdigit.benftest(c)$p.value))}
  )
  
  switch(m,
         d = {benf.test <- benford(d, number.of.digits = 1)},
         b = {benf.test <- benford(b, number.of.digits = 1)},
         l = {benf.test <- benford(l, number.of.digits = 1)},
         c = {benf.test <- benford(c, number.of.digits = 1)}
  )
  
  results <- rbind(results, c('livejournal', m, c(x, benf.test$stats$mantissa.arc.test$p.value, benf.test$MAD, benf.test$distortion.factor)))
}

results <- results[-c(1), ] 

write.csv(x = results, file = 'results.livejournal.csv')
