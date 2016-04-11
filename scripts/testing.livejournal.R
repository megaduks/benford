library(data.table)
library(benford.analysis)
library(BenfordTests)
library(igraph)
library(dplyr)

data.file <- fread(input = 'data/large sets/livejournal.csv', header = FALSE, skip = 4)
names(data.file) <- c('fromID', 'toID')

data.sample <- sample_n(data.file, 10000)
g <- graph.data.frame(data.file, directed = TRUE)
