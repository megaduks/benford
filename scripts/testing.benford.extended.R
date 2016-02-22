library(igraph)
library(ggplot2)
library(data.table)
library(benford.analysis)
library(BenfordTests)
library(tools)

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

results <- data.frame(dataset = NA, measure = NA, chi = NA, ks = NA, f = NA, chd = NA, ed = NA, js = NA, jj = NA, jd = NA)

# get the list of all data files 
data.files <- list.files(path = ".", pattern = "*RData")

for (data.file in data.files) {
   
  # load the data from the file
   load(data.file)
   d <- test.degree$data$data.used
   b <- test.betweenness$data$data.used
   c <- test.closeness$data$data.used
   l <- test.clustering$data$data.used
   
   for (m in c('d', 'b', 'l', 'c')) {
     
     switch(m,
            d = { x = c(chisq.benftest(d)$p.value, ks.benftest(d)$p.value, usq.benftest(d)$p.value, mdist.benftest(d)$p.value, edist.benftest(d)$p.value, meandigit.benftest(d)$p.value, jpsq.benftest(d)$p.value, as.numeric(jointdigit.benftest(d)$p.value))},
            b = { x = c(chisq.benftest(b)$p.value, ks.benftest(b)$p.value, usq.benftest(b)$p.value, mdist.benftest(b)$p.value, edist.benftest(b)$p.value, meandigit.benftest(b)$p.value, jpsq.benftest(b)$p.value, as.numeric(jointdigit.benftest(b)$p.value))},
            l = { x = c(chisq.benftest(l)$p.value, ks.benftest(l)$p.value, usq.benftest(l)$p.value, mdist.benftest(l)$p.value, edist.benftest(l)$p.value, meandigit.benftest(l)$p.value, jpsq.benftest(l)$p.value, as.numeric(jointdigit.benftest(l)$p.value))},
            c = { x = c(chisq.benftest(c)$p.value, ks.benftest(c)$p.value, usq.benftest(c)$p.value, mdist.benftest(c)$p.value, edist.benftest(c)$p.value, meandigit.benftest(c)$p.value, jpsq.benftest(c)$p.value, as.numeric(jointdigit.benftest(c)$p.value))}
     )
     
     results <- rbind(results, c(data.file, m, x))
   }
}
  
results <- results[-c(1), ] 
   
write.csv(x = results, file = 'results.extended.real.networks.csv')