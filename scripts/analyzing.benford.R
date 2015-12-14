library(igraph)
library(ggplot2)
library(data.table)
library(benford.analysis)
library(tools)
library(dplyr)
library(xtable)

# 
# Benford's Law in social networks
#
# author: Mikołaj Morzy
# date: 13-12-2015
#
# The purpose of this script is to analyze the results of test conducted on real world networks as well as on sythetic networks
# generated from popular generative network models. For each of the considered networks several tests of Benford concordance have been performed
#
# - chi squared test of distribution concordance
# - mantissa arc test
# - distortion factor test
# - mean absolute deviation
#
# In the results below we verify if each network has passed the test w.r.t. degree, betweenness, and closeness distributions

# read the test results for real world networks
test.results <- read.csv(file = 'results.real.networks.csv')

# standard deviation of the distortion factor model
sd <- 0.638 / sqrt(10)

# add binary flags if the p-value of chi.square test or mantissa arc test are significant at alpha = 0.05, 
# and if MAD is below 0.0018, and if Pearson's correlation coefficient is above 0.99 and if DF model accepts null hypothesis
test.results <- test.results %>%
  mutate( chi.sq.flag = ifelse(chi.sq.pval >= 0.05, 1, 0)) %>%
  mutate( mat.flag = ifelse(mat.pval >= 0.05, 1, 0)) %>%
  mutate( mad.flag = ifelse(mad <= 0.0018, 1, 0)) %>%
  mutate( df.flag = ifelse( abs(df/sd) <= 1.96, 1, 0)) %>%
  mutate( pcc.flag = ifelse(pcc >= 0.99, 1, 0))

# add a flag which indicates the concordance with Benford's distribution if at least two flags are set
test.results <- 
  test.results %>%
  mutate( flags = (test.results %>% select(contains("flag")) %>% rowSums(.) ))

# generate the final list of datasets and measures which are concordant with Benford's distribution
benford.concordant.real.networks <- test.results %>%
  filter( flags > 1 ) %>%
  select(dataset, measure, flags)


# read the test results for artificial networks
test.results <- read.csv(file = 'results.artificial.networks.csv')

# aggregate the results by computing mean values for a given model and a given parameter value
mean.test.results <- test.results %>%
  group_by(model, parameter.set.j., measure) %>%
  summarise_each(funs(mean))

# add binary flags if the p-value of chi.squared test or mantissa arc test are significant at alpha = 0.95,
# and if MAD is below 0.0018
mean.test.results <- mean.test.results %>%
  mutate( chi.sq.flag = ifelse(tr.stats.chisq.p.value >= 0.05, 1, 0) )  %>%
  mutate( mat.flag = ifelse(tr.stats.mantissa.arc.test.p.value >= 0.05, 1, 0) ) %>%
  mutate( mad.flag = ifelse(tr.MAD <= 0.0018, 1, 0)) %>%
  mutate( df.flag = ifelse( abs(tr.distortion.factor/sd) <= 1.96, 1, 0))

# add a flag which indicates the concordance with Benford's distribution if at least one flag is set
mean.test.results$flags <- rowSums(mean.test.results[,10:13])


# generate the final list of datasets and measures which are concordant with Benford's distribution
benford.concordant.artificial.networks <- mean.test.results %>%
  filter(flags >= 2) %>%
  select(model, parameter.set.j., measure)

# read the results of tests on real world networks and generate figures
file.list <- list.files(path = '.', pattern = '*RData')

for (file in file.list) {
  load(file)
  
  measures <- c('degree', 'betweenness', 'clustering', 'closeness')
  for (measure in measures) {
    
    file.name <- paste(file_path_sans_ext(file), measure, 'pdf', sep = '.')
    pdf(file = paste('figures/', file.name), width = 7, height = 5)
    
    test.name <- paste('test',measure, sep = '.')
    plot(get(test.name), except = c('mantissa', 'chi square', 'abs diff', 'ex summation'))
    dev.off()
  }
}
