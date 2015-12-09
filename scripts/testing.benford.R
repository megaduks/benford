library(igraph)
library(ggplot2)
library(data.table)
library(benford.analysis)

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
#  * alpha centrality distribution (for alpha = 0.5)
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

# get the list of all data files 
data.files <- list.files(path = "data/")

# the first loop iterates over all files

for (data.file in data.files) {
  
  # read the data file and transform it to an igraph object
  g <- get.data.from.file(data.file)
  
  # compute the number of vertices and edges
  num.nodes <- vcount(g)
  num.edges <- ecount(g)

  # compute the distribution of degrees and remove all isolated nodes
  d <- degree(g, normalized = FALSE)
  d <- d[d > 0]
  
  # compute the distribution of degree first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.degree <- table(sapply(d, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.degree)))
  benford <- round(num.nodes * benford)
  degree.pcc <- cor(fd.degree, benford)
  
  # compute the distribution of betweenness and remove all nodes with zero betweenness
  b <- betweenness(g, directed = FALSE)
  b <- b[b > 0]
  
  # compute the distribution of betweenness first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.betweenness <- table(sapply(b, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.betweenness)))
  benford <- round(num.nodes * benford)
  betweenness.pcc <- cor(fd.betweenness, benford)
    
  # compute the distribution of the local clustering coefficient and retain only non-null and non-zero values
  c <- transitivity(g, type = 'local', isolates = 'zero')
  c <- c[!is.nan(c) & c > 0]
  if (length(c) == 0)
    c <- c(0)
  
  # compute the distribution of local clustering coefficient first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.clustering <- table(sapply(c, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.clustering)))
  benford <- round(num.nodes * benford)
  clustering.pcc <- cor(fd.clustering, benford)
  
  # compute the distribution of the alpha centrality for a fixed alpha = 0.5 and retain only non-zero values
  a <- alpha.centrality(g, alpha = 0.5)
  a <- a[a > 0]
  
  # compute the distribution of degree first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
  fd.alpha <- table(sapply(a, first.digit))
  benford <- log10(1 + 1/seq(1:length(fd.alpha)))
  benford <- round(num.nodes * benford)
  alpha.pcc <- cor(fd.alpha, benford)
  
  # perform actual test of Bedford compliance on the four centrality measures
  test.degree       <- benford(d, number.of.digits = 1, discrete = TRUE)
  test.betweenness  <- benford(b, number.of.digits = 1, discrete = TRUE)
  test.clustering   <- benford(c, number.of.digits = 1, discrete = TRUE)
  test.alpha        <- benford(a, number.of.digits = 1, discrete = TRUE)
  
  # fill the data frame with results of all four tests
  test.degree.result <- c(data.file, num.nodes, num.edges, "degree", test.degree$stats$chisq$statistic, test.degree$stats$chisq$p.value, test.degree$MAD, 
                          test.degree$stats$mantissa.arc.test$statistic, test.degree$stats$mantissa.arc.test$p.value, test.degree$distortion.factor, degree.pcc)
  test.betweenness.result <- c(data.file, num.nodes, num.edges, "betweenness", test.betweenness$stats$chisq$statistic, test.betweenness$stats$chisq$p.value, test.betweenness$MAD, 
                          test.betweenness$stats$mantissa.arc.test$statistic, test.betweenness$stats$mantissa.arc.test$p.value, test.betweenness$distortion.factor, betweenness.pcc)
  test.cluster.result <- c(data.file, num.nodes, num.edges, "clustering coefficient", test.clustering$stats$chisq$statistic, test.clustering$stats$chisq$p.value, test.clustering$MAD, 
                          test.clustering$stats$mantissa.arc.test$statistic, test.clustering$stats$mantissa.arc.test$p.value, test.clustering$distortion.factor, clustering.pcc)
  test.alpha.result <- c(data.file, num.nodes, num.edges, "alpha centrality", test.alpha$stats$chisq$statistic, test.alpha$stats$chisq$p.value, test.alpha$MAD, 
                          test.alpha$stats$mantissa.arc.test$statistic, test.alpha$stats$mantissa.arc.test$p.value, test.alpha$distortion.factor, alpha.pcc)
  
  results <- rbind(results, test.degree.result, test.betweenness.result, test.cluster.result, test.alpha.result)
}

