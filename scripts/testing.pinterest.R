library(data.table)
library(benford.analysis)
library(BenfordTests)


data.file <- fread(input = 'data/large sets/pinterest.csv', header = FALSE, sep = '|')
names(data.file) <- c('userID','boards','pins','likes','followers','followed')

data.sample <- sample(data.file$pins, size = 10000000)
pins.result <- benford(data.sample, number.of.digits = 1)

data.sample <- sample(data.file$likes, size = 10000000)
likes.result <- benford(data.sample, number.of.digits = 1)

data.sample <- sample(data.file$followers, size = 10000000)
followers.result <- benford(data.sample, number.of.digits = 1)

data.sample <- sample(data.file$followed, size = 10000000)
followed.result <- benford(data.sample, number.of.digits = 1)
