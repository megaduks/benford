library(igraph)
library(ggplot2)
library(data.table)
library(benford.analysis)
library(BenfordTests)
library(tools)


# 
# Benford's Law in social networks
#
# author: Mikołaj Morzy
# date: 9-12-2015
#
# The purpose of this script is to evaluate the claims that Benford's distribution of the most significant digit is pronounced
# in several social network properties, in particular, in the distribution of node degrees.
# We first examine 10 different real-world datasets listed below, and then we conduct experiments on synthetical networks
# obtained from generative network models popular in the literature. For each network we compute:
#
#  * degree distribution
#  * betweenness distribution
#  * local clustering coefficient distribution
#
# Ddatasets used in the experiment were only slightly modified w.r.t. original SNAP data repository
# spaces between attributes were changed into tablulations
# Datasets used in experiments include: 
#   data/enron.csv
#   data/wikipedia.csv
#   data/citations.csv
#   data/amazon.csv
#   data/gnutella.csv
#   data/physics.csv
#   data/twitter.csv
#   data/dblp.csv
#   data/slashdot.csv
#   data/facebook.csv

#
# Function determines the first most significant digit in a number that is other than zero.
# The function works by computing the mantissa of the number and extracting the first digit 
# of the mantissa
#
# parameters
#   x - number for which the most significant nono-zero digit is to be found
#
# result
#   most.significant.digit - the most significant non-zero digit

first.digit <- function(x) {
  # epsilon is required to avoid NaN when computing the logarithm to find the mantissa of the number
  epsilon <- 0.0000000001
  
  mantissa <- 10^(-round(log10(x + epsilon))+1) * x
  
  most.significant.digit  <- as.numeric(unlist(strsplit(as.character(mantissa), ''))[1])
  most.significant.digit
}


#
# Helper function to read the graph from the CSV file 
# The default format of the file are two columns separated by a TAB character
#
# parameters
#   filename - the name of the file containing data to be read
#
# result
#   g - igraph object containing an undirected graph created from CSV file

get.data.from.file <- function(filename) {
  # the name of the directory where data is stored
  data.directory <- "data/"
  # full path to the file containing data
  filename <- paste(data.directory, filename, sep = '')
  
  data.file <- fread(input = filename, header = FALSE, sep = '\t')
  g <- graph.data.frame(data.file, directed = FALSE)
  g
}

#
# Helper function to generate artificial graph from one of the available generative network models. Each network is generated using 
# a particular value of the main parameter of its generative network model. Available models and their main parameters include: 
#   * a random graph with edge creation probability
#   * a small world network with rewiring probability
#   * a preferential attachment network with alpha coefficient of the power distribution
#   * a forest fire network with the forward burning probability
# 
#  parameters
#    name - the name of the generative model [random.graph, small.world, preferential.attachment, forest.fire]
#    nodes - the number of vertices in the generated network
#    param - the value of the main parameter of a given generative network model
#
#  result
#    g - igraph object containing an undirected graph created by the generative model

get.data.synthetic <- function(name, nodes, param) {
  # seed K4 graph to boost the network created by the preferential attachment model
  seed.graph <- make_full_graph(4)
  
  switch(name,
         random.graph =            {g <- sample_gnp(n = nodes, p = param, directed = FALSE)},
         small.world =             {g <- sample_smallworld(dim = 1, size = nodes, nei = 4, p = param)},
         preferential.attachment = {g <- sample_pa(n = nodes, power = param, directed = FALSE, start.graph = seed.graph)},
         forest.fire =             {g <- sample_forestfire(nodes = nodes, fw.prob = param, directed = FALSE)}
  )
  g
}

# 
# Helper function to prune distributions of centrality measures by removing entries equal to either 0 or Nan
#
#  parameters
#    x - the numerical vector to be pruned
#
#  result
#    x - the numerical vector with 0s and Nans removed

prune.distribution <- function(x) {
  x <- x[!is.nan(x) & x > 0]
  if (length(x) == 0)
    x <- c(1)
  x
}

#
# The dataframe containing all the results of conducted measurements. The columns are the following:
#   dataset - the name of the dataset
#   num.vertices - the number of vertices in the graph
#   num.edges - the number of edges in the graph
#   measure - the name of the measure [degree, betweenness, clustering.coefficient, alpha.centrality]
#   chi.sq - the value of the Chi-Square statistic
#   chi.sq.pval - the p-value of the Chi-Square test
#   mad - Mean Absolute Deviation
#   mat - Mantissa Arc Test length of the vector to the center of the mass
#   mat.pval - the p-value of the significance of the test of center of the mass location
#   df - Distortion Factor
#   pcc - Pearson's correlation coefficient

results <- data.frame(dataset = NA, num.vertices = NA, num.edges = NA, measure = NA, chi.sq = NA, chi.sq.pval = NA, mad = NA, mat = NA, mat.pval = NA, df = NA, pcc = NA)
# 
# # get the list of all data files 
# data.files <- list.files(path = "data/", pattern = "*csv")

# the first loop iterates over all files with real networks and for each network it creates the igraph object, computes the distribution of degrees, betweenness, and 
# clustering coefficients, and performs the Benford concordance tests.

for (data.file in data.files) {
  
<<<<<<< HEAD
  # read the data file and transform it to an igraph object
  g <- get.data.from.file(data.file)
  
  # compute the number of vertices and edges
  num.nodes <- vcount(g)
  num.edges <- ecount(g)
=======
#   # compute the distribution of closeness first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
#   fd.closeness <- table(sapply(l, first.digit))
#   benford <- log10(1 + 1/seq(1:length(fd.closeness)))
#   benford <- round(num.nodes * benford)
#   closeness.pcc <- cor(fd.closeness, benford)
#   
#   # compute the distribution of the local clustering coefficient and retain only non-null and non-zero values
#   c <- transitivity(g, type = 'local', isolates = 'zero')
#   c <- prune.distribution(c)
#   
#   # compute the distribution of local clustering coefficient first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
#   fd.clustering <- table(sapply(c, first.digit))
#   benford <- log10(1 + 1/seq(1:length(fd.clustering)))
#   benford <- round(num.nodes * benford)
#   clustering.pcc <- cor(fd.clustering, benford)
#   
#   
#   # perform actual test of Bedford compliance on the four centrality measures
#   test.degree       <- benford(d, number.of.digits = 1, discrete = TRUE)
#   test.betweenness  <- benford(b, number.of.digits = 1, discrete = TRUE)
#   test.clustering   <- benford(c, number.of.digits = 1, discrete = TRUE)
#   test.closeness    <- benford(l, number.of.digits = 1, discrete = TRUE)
#   
#   # save test results so that further we can generate figures
#   save(test.degree, test.betweenness, test.clustering, test.closeness, file = paste(file_path_sans_ext(data.file),"tests","RData", sep = "."))
#   
#   # fill the data frame with results of all four tests
#   test.degree.result <- c(data.file, num.nodes, num.edges, "degree", test.degree$stats$chisq$statistic, test.degree$stats$chisq$p.value, test.degree$MAD, 
#                           test.degree$stats$mantissa.arc.test$statistic, test.degree$stats$mantissa.arc.test$p.value, test.degree$distortion.factor, degree.pcc)
#   test.betweenness.result <- c(data.file, num.nodes, num.edges, "betweenness", test.betweenness$stats$chisq$statistic, test.betweenness$stats$chisq$p.value, test.betweenness$MAD, 
#                           test.betweenness$stats$mantissa.arc.test$statistic, test.betweenness$stats$mantissa.arc.test$p.value, test.betweenness$distortion.factor, betweenness.pcc)
#   test.cluster.result <- c(data.file, num.nodes, num.edges, "clustering coefficient", test.clustering$stats$chisq$statistic, test.clustering$stats$chisq$p.value, test.clustering$MAD, 
#                           test.clustering$stats$mantissa.arc.test$statistic, test.clustering$stats$mantissa.arc.test$p.value, test.clustering$distortion.factor, clustering.pcc)
#   test.closeness.result <- c(data.file, num.nodes, num.edges, "closeness", test.closeness$stats$chisq$statistic, test.closeness$stats$chisq$p.value, test.closeness$MAD, 
#                            test.closeness$stats$mantissa.arc.test$statistic, test.closeness$stats$mantissa.arc.test$p.value, test.closeness$distortion.factor, closeness.pcc)
#   
#   # save test results into the final data frame
#   results <- rbind(results, test.degree.result, test.betweenness.result, test.cluster.result, test.closeness.result)
# 
#   # remove the first row of the results data fraome (contains only initial NULLs)
#   results <- results[-c(1), ]  
#   results <- transform(results, num.vertices = as.numeric(num.vertices), num.edges = as.numeric(num.edges), chi.sq = as.numeric(chi.sq), chi.sq.pval = as.numeric(chi.sq.pval), mad = as.numeric(mad), mat = as.numeric(mat), mat.pval = as.numeric(mat.pval), df = as.numeric(df), pcc = as.numeric(pcc))
# }
# 
# # save the results to the file
# write.csv(x = results, file = 'results.real.networks.csv')



# the second loop iterates over all generative network models. for each network model 5 different network parameters are selected, and for each parameter 100 realizations of 
# a synthetic network are generated. the results of Benford concordance test is averaged over all 100 realizations of a particular network model with a particular main parameter setting

# the list of generative artificial network models
generative.models <- c("random.graph", "small.world", "preferential.attachment", "forest.fire")

# number of vertices to be created in artificial graphs
num.vertices <- 10000

# number of realizations of each network
num.graphs <- 100

# parameters for generative network models
edge.probability <- seq(0.0001, 0.001, length.out = 10)
rewire.probability <- seq(0.001, 0.005, length.out = 10)
alpha.coefficient <- seq(1, 3, length.out = 10)
forward.burning <- seq(0.01, 0.25, length.out = 10)

network.parameters <- rbind(edge.probability, rewire.probability, alpha.coefficient, forward.burning)

# data frame to store the results of all generative network model experiments
test.results <- data.frame(nodel.name = character(), param.value = numeric(), measure = character(), chisq.stat = numeric(), chisq.pval = numeric(), mad = numeric(), mat.stat = numeric(), mat.pval = numeric(), df = numeric())
>>>>>>> 3921b8391f840da435da0fbbbb93b103c50b1e1e

  # compute the distribution of degrees and remove all isolated nodes
  d <- degree(g, normalized = FALSE)
  d <- prune.distribution(d)
  
  # compute the distribution of degree first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.degree <- table(sapply(d, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.degree)))
  benford <- round(num.nodes * benford)
  degree.pcc <- cor(fd.degree, benford)
  
  # compute the distribution of betweenness and remove all nodes with zero betweenness
  b <- betweenness(g, directed = FALSE)
  b <- prune.distribution(b)
  
  # compute the distribution of betweenness first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.betweenness <- table(sapply(b, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.betweenness)))
  benford <- round(num.nodes * benford)
  betweenness.pcc <- cor(fd.betweenness, benford)
  
  # compute the distribution of closeness and remove all nodes with zero closeness
  # estimation is required for larger graphs due to very high computational cost of measuring closeness centrality
  
#   if (num.nodes < 30000)
#     l <- closeness(g, mode = 'all')
#   else if (num.nodes < 50000)
#     l <- estimate_closeness(g, mode = 'all', cutoff = 5)
#   else
#     l <- estimate_closeness(g, mode = 'all', cutoff = 3)
  
  l <- closeness(g, mode = 'all')
  l <- prune.distribution(l)
  
  # compute the distribution of closeness first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.closeness <- table(sapply(l, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.closeness)))
  benford <- round(num.nodes * benford)
  closeness.pcc <- cor(fd.closeness, benford)
  
  # compute the distribution of the local clustering coefficient and retain only non-null and non-zero values
  c <- transitivity(g, type = 'local', isolates = 'zero')
  c <- prune.distribution(c)
  
  # compute the distribution of local clustering coefficient first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.clustering <- table(sapply(c, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.clustering)))
  benford <- round(num.nodes * benford)
  clustering.pcc <- cor(fd.clustering, benford)
  
  
  # perform actual test of Bedford compliance on the four centrality measures
  test.degree       <- benford(d, number.of.digits = 1, discrete = TRUE)
  test.betweenness  <- benford(b, number.of.digits = 1, discrete = TRUE)
  test.clustering   <- benford(c, number.of.digits = 1, discrete = TRUE)
  test.closeness    <- benford(l, number.of.digits = 1, discrete = TRUE)
  
  # save test results so that further we can generate figures
  save(test.degree, test.betweenness, test.clustering, test.closeness, file = paste(file_path_sans_ext(data.file),"tests","RData", sep = "."))
  
  # fill the data frame with results of all four tests
  test.degree.result <- c(data.file, num.nodes, num.edges, "degree", test.degree$stats$chisq$statistic, test.degree$stats$chisq$p.value, test.degree$MAD, 
                          test.degree$stats$mantissa.arc.test$statistic, test.degree$stats$mantissa.arc.test$p.value, test.degree$distortion.factor, degree.pcc)
  test.betweenness.result <- c(data.file, num.nodes, num.edges, "betweenness", test.betweenness$stats$chisq$statistic, test.betweenness$stats$chisq$p.value, test.betweenness$MAD, 
                          test.betweenness$stats$mantissa.arc.test$statistic, test.betweenness$stats$mantissa.arc.test$p.value, test.betweenness$distortion.factor, betweenness.pcc)
  test.cluster.result <- c(data.file, num.nodes, num.edges, "clustering coefficient", test.clustering$stats$chisq$statistic, test.clustering$stats$chisq$p.value, test.clustering$MAD, 
                          test.clustering$stats$mantissa.arc.test$statistic, test.clustering$stats$mantissa.arc.test$p.value, test.clustering$distortion.factor, clustering.pcc)
  test.closeness.result <- c(data.file, num.nodes, num.edges, "closeness", test.closeness$stats$chisq$statistic, test.closeness$stats$chisq$p.value, test.closeness$MAD, 
                           test.closeness$stats$mantissa.arc.test$statistic, test.closeness$stats$mantissa.arc.test$p.value, test.closeness$distortion.factor, closeness.pcc)
  
  # save test results into the final data frame
  results <- rbind(results, test.degree.result, test.betweenness.result, test.cluster.result, test.closeness.result)

  # remove the first row of the results data fraome (contains only initial NULLs)
  results <- results[-c(1), ]  
  results <- transform(results, num.vertices = as.numeric(num.vertices), num.edges = as.numeric(num.edges), chi.sq = as.numeric(chi.sq), chi.sq.pval = as.numeric(chi.sq.pval), mad = as.numeric(mad), mat = as.numeric(mat), mat.pval = as.numeric(mat.pval), df = as.numeric(df), pcc = as.numeric(pcc))
}

# save the results to the file
write.csv(x = results, file = 'results.real.networks.citations.csv')



# the second loop iterates over all generative network models. for each network model 5 different network parameters are selected, and for each parameter 100 realizations of 
# a synthetic network are generated. the results of Benford concordance test is averaged over all 100 realizations of a particular network model with a particular main parameter setting

# the list of generative artificial network models
# generative.models <- c("random.graph", "small.world", "preferential.attachment", "forest.fire")
# 
# 
# # number of vertices to be created in artificial graphs
# num.vertices <- 10000
# 
# # number of realizations of each network
# num.graphs <- 100
# 
# # parameters for generative network models
# edge.probability <- seq(0.0001, 0.001, length.out = 10)
# rewire.probability <- seq(0.001, 0.005, length.out = 10)
# alpha.coefficient <- seq(1, 3, length.out = 10)
# forward.burning <- seq(0.01, 0.25, length.out = 10)
# 
# network.parameters <- rbind(edge.probability, rewire.probability, alpha.coefficient, forward.burning)
# 
# # data frame to store the results of all generative network model experiments
# test.results <- data.frame(nodel.name = character(), param.value = numeric(), measure = character(), chisq.stat = numeric(), chisq.pval = numeric(), mad = numeric(), mat.stat = numeric(), mat.pval = numeric(), df = numeric())
# 
# for (i in 1:length(generative.models)) {
#   # read the name of the model
#   model <- generative.models[i]
#   
#   # read the parameter set for the model
#   parameter.set <- network.parameters[i,]
#   
#   # iterate over each value of the main parameter
#   for (j in 1:length(parameter.set)) {
#     
#     # generate num.graphs artificial graphs according to the given generative model and its main parameter
#     graphs <- lapply(rep(model, num.graphs), get.data.synthetic, num.vertices, parameter.set[j])
#     
#     # compute the distribution of degrees
#     d <- lapply(graphs, degree, normalized = FALSE)
#     d <- lapply(d, prune.distribution)
#     
#     # test the concordance of the Benford distribution to the distribution of node degrees
#     test.degree <- lapply(d, benford, number.of.digits = 1, discrete = TRUE)
#     
#     # iterate over all test results and read the values of these results into vectors
#     for (k in 1:num.graphs) {
#       tr <- test.degree[[k]]
#       tr.values <- data.frame(model, parameter.set[j], measure = "degree", tr$stats$chisq$statistic, tr$stats$chisq$p.value, tr$MAD, tr$stats$mantissa.arc.test$statistic, tr$stats$mantissa.arc.test$p.value, tr$distortion.factor)
#       test.results <- rbind(test.results, tr.values)
#     }
#     
#     # compute the distribution of betweenness
#     b <- lapply(graphs, betweenness, directed = FALSE)
#     b <- lapply(b, prune.distribution)
#     
#     # test the concordance of the Benford distribution to the distribution of node betweenness
#     test.betweenness <- lapply(b, benford, number.of.digits = 1, discrete = TRUE)
#     
#     # iterate over all test results and read the values of these results into vectors
#     for (k in 1:num.graphs) {
#       tr <- test.betweenness[[k]]
#       tr.values <- data.frame(model, parameter.set[j], measure = "betweenness", tr$stats$chisq$statistic, tr$stats$chisq$p.value, tr$MAD, tr$stats$mantissa.arc.test$statistic, tr$stats$mantissa.arc.test$p.value, tr$distortion.factor)
#       test.results <- rbind(test.results, tr.values)
#     }
#     
#     # compute the distribution of closeness
#     c <- lapply(graphs, closeness, mode = 'all')
#     c <- lapply(c, prune.distribution)
#     
#     # test the concordance of the Benford distribution to the distribution of node closeness
#     test.closeness <- lapply(c, benford, number.of.digits = 1, discrete = TRUE)
#     
#     # iterate over all test results and read the values of these results into vectors
#     for (k in 1:num.graphs) {
#       tr <- test.closeness[[k]]
#       tr.values <- data.frame(model, parameter.set[j], measure = "closeness", tr$stats$chisq$statistic, tr$stats$chisq$p.value, tr$MAD, tr$stats$mantissa.arc.test$statistic, tr$stats$mantissa.arc.test$p.value, tr$distortion.factor)
#       test.results <- rbind(test.results, tr.values)
#     }
#     
#     # compute the distribution of local clustering coefficient
#     c <- lapply(graphs, transitivity, type = 'local')
#     c <- lapply(c, prune.distribution)
#     
#     # test the concordance of the Benford distribution to the distribution of node local clustering coefficient
#     test.clustering <- lapply(c, benford, number.of.digits = 1, discrete = TRUE)
#     
#     # iterate over all test results and read the values of these results into vectors
#     for (k in 1:num.graphs) {
#       tr <- test.clustering[[k]]
#       tr.values <- data.frame(model, parameter.set[j], measure = "clustering", tr$stats$chisq$statistic, tr$stats$chisq$p.value, tr$MAD, tr$stats$mantissa.arc.test$statistic, tr$stats$mantissa.arc.test$p.value, tr$distortion.factor)
#       test.results <- rbind(test.results, tr.values)
#     }
#   }
#   
#   # remove the first row of the results data fraome (contains only initial NULLs)
# #   test.results <- test.results[-c(1), ]  
# #   test.results <- transform(test.results, num.vertices = as.numeric(num.vertices), num.edges = as.numeric(num.edges), chi.sq = as.numeric(chi.sq), chi.sq.pval = as.numeric(chi.sq.pval), mad = as.numeric(mad), mat = as.numeric(mat), mat.pval = as.numeric(mat.pval), df = as.numeric(df), pcc = as.numeric(pcc))
# }
# 
# # save the results to the file
# write.csv(x = test.results, file = 'results.artificial.networks.csv')
# 
