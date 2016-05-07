library(BenfordTests)
library(benford.analysis)
library(dplyr)
library(ggplot2)

# size of the sample
n <- 10000



# ratio of Benford's distribution contamination with Normal samples
ratio <- seq(0, 1, by = 0.01)

results <- data.frame(i = numeric(), test = character(), test.number = character(), pval = numeric(), statistic = numeric()) 

for (i in ratio) {
  for (j in 1:50) {
    
    # pure samples of Benford and Normal distributions
    benford.sample <- rbenf(n)
    normal.sample <- abs(rnorm(n = n, mean = 4, sd = 2))
    
    # mixture of Benford and Normal distributions
    data.sample <- c( sample(benford.sample, n*i), sample(normal.sample, n*(1-i)))
    
    t <- chisq.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t1", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- edist.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t2", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- jointdigit.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t3", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- jpsq.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t4", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- ks.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t5", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- mdist.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t6", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- usq.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t7", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- meandigit.benftest(data.sample)
    d <- data.frame(i, test = t$method, test.number = "t8", pval = t$p.value, statistic = t$statistic)
    results <- rbind(results, d)
    
    t <- benford(data.sample, number.of.digits = 1)
    d <- data.frame(i, test = t$stats$mantissa.arc.test$method, test.number = "t9", pval = t$stats$mantissa.arc.test$p.value, statistic = t$stats$mantissa.arc.test$statistic)
    results <- rbind(results, d)
    d <- data.frame(i, test = "Mean Absolute Deviation", test.number = "t10", pval = NA, statistic = t$MAD)
    results <- rbind(results, d)
    d <- data.frame(i, test = "Distortion Factor", test.number = "t11", pval = NA, statistic = t$distortion.factor)
    results <- rbind(results, d)
  }
}

row.names(results) <- NULL

avg.results <- results %>%
  group_by(i, test, test.number) %>%
  summarize(avg.pval = mean(pval), avg.stat = mean(statistic))

avg.results <- avg.results %>%
  mutate( flag = ifelse(avg.pval>=0.05 & test.number=='t1', 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t2') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t3') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t4') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t5') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t6') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t7') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t8') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.pval>=0.05 & test.number=='t9') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((avg.stat<=0.0018 & test.number=='t10') | flag==1, 1, 0)) %>%
  mutate( flag = ifelse((abs((avg.stat/100)/(0.638/sqrt(n))) <= 1.96 & test.number=='t11') | flag==1, 1, 0))

ggplot(avg.results[!(avg.results$test.number %in% c('t10','t11')), ], aes(x = i, y = avg.pval, colour = test.number)) + 
  geom_line() + geom_point() + xlim(0.75, 1) + xlab('purity of Benford\'s distribution') + ylab('average test p-value') +
  geom_hline(aes(yintercept = 0.05), colour = 'red', linetype = 2) + scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5))
