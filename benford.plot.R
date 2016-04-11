library(ggplot2)

x <- rnorm(1000, mean = 3, sd = 1.1)
dens <- density(x)

dd <- with(dens, data.frame(x,y))

tics <- 0:6
tics <- c(tics, tics+0.301)

qplot(x, y, data = dd, geom = "line") + scale_x_continuous(breaks = tics) +
  geom_ribbon(data = subset(dd, x >= 0 & x < 0.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 1 & x < 1.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 2 & x < 2.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 3 & x < 3.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 4 & x < 4.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 5 & x < 5.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5) +
  geom_ribbon(data = subset(dd, x >= 6 & x < 6.301), aes(ymax = y), ymin = 0, fill = "dimgray", alpha = 0.5)
