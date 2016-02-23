
library(igraph)
library(ggplot2)
library(data.table)
library(benford.analysis)
library(BenfordTests)
library(tools)


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

# the list of generative artificial network models
#generative.models <- c("random.graph", "small.world", "preferential.attachment", "forest.fire")
generative.models <- c("forest.fire")


# number of vertices to be created in artificial graphs
num.vertices <- 1000

# number of realizations of each network
num.graphs <- 50

# parameters for generative network models
edge.probability <- seq(0.001, 0.01, length.out = 10)
rewire.probability <- seq(0.01, 0.05, length.out = 10)
alpha.coefficient <- seq(1, 3, length.out = 10)
forward.burning <- seq(0.01, 0.10, length.out = 10)

network.parameters <- rbind(edge.probability, rewire.probability, alpha.coefficient, forward.burning)

# data frame to store the results of all generative network model experiments
results <- data.frame(dataset = character(), param = numeric(), measure = character(), chi = numeric(), ks = numeric() , f = numeric(), chd = numeric(), ed = numeric() , js = numeric(), jj = numeric(), jd = numeric())

for (i in 1:length(generative.models)) {
  # read the name of the model
  model <- generative.models[i]
  
  # read the parameter set for the model
  parameter.set <- network.parameters[i,]
  
  # iterate over each value of the main parameter
  for (j in 1:length(parameter.set)) {
    
    # generate num.graphs artificial graphs according to the given generative model and its main parameter
    graphs <- lapply(rep(model, num.graphs), get.data.synthetic, num.vertices, parameter.set[j])
    
    # compute centrality measures
    d <- lapply(graphs, degree, normalized = FALSE)
    d <- lapply(d, prune.distribution)
    b <- lapply(graphs, betweenness, directed = FALSE)
    b <- lapply(b, prune.distribution)
    c <- lapply(graphs, closeness, mode = 'all')
    c <- lapply(c, prune.distribution)
    l <- lapply(graphs, transitivity, type = 'local')
    l <- lapply(l, prune.distribution)
    
    d.chisq <- lapply(d, chisq.benftest)
    d.chisq.test <-  sum(sapply(d.chisq, with, p.value) >= 0.05)
    
    d.ks <- lapply(d, ks.benftest)
    d.ks.test <-     sum(sapply(d.ks, with, p.value) >= 0.05)
    
    d.usq <- lapply(d, usq.benftest)
    d.usq.test <-  sum(sapply(d.usq, with, p.value) >= 0.05)
    
    d.mdist <- lapply(d, mdist.benftest)
    d.mdist.test <-  sum(sapply(d.mdist, with, p.value) >= 0.05)
    
    d.edist <- lapply(d, edist.benftest)
    d.edist.test <-  sum(sapply(d.edist, with, p.value) >= 0.05)
    
    d.meandigit <- lapply(d, meandigit.benftest)
    d.meandigit.test <-  sum(sapply(d.meandigit, with, p.value) >= 0.05)
    
    d.jpsq <- lapply(d, jpsq.benftest)
    d.jpsq.test <-  sum(sapply(d.jpsq, with, p.value) >= 0.05)
    
    d.jointdigit <- lapply(d, jointdigit.benftest)
    d.jointdigit.test <-  sum(sapply(d.jointdigit, with, p.value) >= 0.05)
    
    res <- data.frame(dataset=model, param=parameter.set[j], measure="degree", chi=d.chisq.test, ks=d.ks.test, f=d.usq.test, chd=d.mdist.test, ed=d.edist.test, js=d.meandigit.test, jj=d.jpsq.test, jd=d.jointdigit.test)
    results <- rbind(results, res)
    
    b.chisq <- lapply(b, chisq.benftest)
    b.chisq.test <-  sum(sapply(b.chisq, with, p.value) >= 0.05)
    
    b.ks <- lapply(b, ks.benftest)
    b.ks.test <-  sum(sapply(b.ks, with, p.value) >= 0.05)
    
    b.usq <- lapply(b, usq.benftest)
    b.usq.test <-  sum(sapply(b.usq, with, p.value) >= 0.05)
    
    b.mdist <- lapply(b, mdist.benftest)
    b.mdist.test <-  sum(sapply(b.mdist, with, p.value) >= 0.05)
    
    b.edist <- lapply(b, edist.benftest)
    b.edist.test <-  sum(sapply(b.edist, with, p.value) >= 0.05)
    
    b.meandigit <- lapply(b, meandigit.benftest)
    b.meandigit.test <-  sum(sapply(b.meandigit, with, p.value) >= 0.05)
    
    b.jpsq <- lapply(b, jpsq.benftest)
    b.jpsq.test <-  sum(sapply(b.jpsq, with, p.value) >= 0.05)
    
    b.jointdigit <- lapply(b, jointdigit.benftest)
    b.jointdigit.test <-  sum(sapply(b.jointdigit, with, p.value) >= 0.05)
    
    res <- data.frame(dataset=model, param=parameter.set[j], measure="betweenness", chi=b.chisq.test, ks=b.ks.test, f=b.usq.test, chd=b.mdist.test, ed=b.edist.test, js=b.meandigit.test, jj=b.jpsq.test, jd=b.jointdigit.test)
    results <- rbind(results, res)
    
    c.chisq <- lapply(c, chisq.benftest)
    c.chisq.test <-  sum(sapply(c.chisq, with, p.value) >= 0.05)
    
    c.ks <- lapply(c, ks.benftest)
    c.ks.test <-  sum(sapply(c.ks, with, p.value) >= 0.05)
   
    c.usq <- lapply(c, usq.benftest)
    c.usq.test <-  sum(sapply(c.usq, with, p.value) >= 0.05)
    
    c.mdist <- lapply(c, mdist.benftest)
    c.mdist.test <-  sum(sapply(c.mdist, with, p.value) >= 0.05)
    
    c.edist <- lapply(c, edist.benftest)
    c.edist.test <-  sum(sapply(c.edist, with, p.value) >= 0.05)
    
    c.meandigit <- lapply(c, meandigit.benftest)
    c.meandigit.test <-  sum(sapply(c.meandigit, with, p.value) >= 0.05)
    
    c.jpsq <- lapply(c, jpsq.benftest)
    c.jpsq.test <-  sum(sapply(c.jpsq, with, p.value) >= 0.05)
    
    c.jointdigit <- lapply(c, jointdigit.benftest)
    c.jointdigit.test <-  sum(sapply(c.jointdigit, with, p.value) >= 0.05)
    
    res <- data.frame(dataset=model, param=parameter.set[j], measure="closeness", chi=c.chisq.test, ks=c.ks.test, f=c.usq.test, chd=c.mdist.test, ed=c.edist.test, js=c.meandigit.test, jj=c.jpsq.test, jd=c.jointdigit.test)
    results <- rbind(results, res)
    
    l.chisq <- lapply(l, chisq.benftest)
    l.chisq.test <-  sum(sapply(l.chisq, with, p.value) >= 0.05)
    
    l.ks <- lapply(l, ks.benftest)
    l.ks.test <-  sum(sapply(l.ks, with, p.value) >= 0.05)
   
    l.usq <- lapply(l, usq.benftest)
    l.usq.test <-  sum(sapply(l.usq, with, p.value) >= 0.05)
    
    l.mdist <- lapply(l, mdist.benftest)
    l.mdist.test <-  sum(sapply(l.mdist, with, p.value) >= 0.05)
    
    l.edist <- lapply(l, edist.benftest)
    l.edist.test <-  sum(sapply(l.edist, with, p.value) >= 0.05)
    
    l.meandigit <- lapply(l, meandigit.benftest)
    l.meandigit.test <-  sum(sapply(l.meandigit, with, p.value) >= 0.05)
    
    l.jpsq <- lapply(l, jpsq.benftest)
    l.jpsq.test <-  sum(sapply(l.jpsq, with, p.value) >= 0.05)
    
    l.jointdigit <- lapply(l, jointdigit.benftest)
    l.jointdigit.test <-  sum(sapply(l.jointdigit, with, p.value) >= 0.05)
    
    res <- data.frame(dataset=model, param=parameter.set[j], measure="clustering", chi=l.chisq.test, ks=l.ks.test, f=l.usq.test, chd=l.mdist.test, ed=l.edist.test, js=l.meandigit.test, jj=l.jpsq.test, jd=l.jointdigit.test)
    results <- rbind(results, res)
  }
}

# save the results to the file
write.csv(x = results, file = 'results.extended.artificial.networks2.csv')
