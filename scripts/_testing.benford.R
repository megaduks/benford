#
# Helper script 
# -------------
# File contains snippets of code that are not currently being used in the main script

fd.degree      <- table(sapply(d, first.digit))
fd.betweenness <- table(sapply(b, first.digit))
fd.clustering  <- table(sapply(cc, first.digit))

benford <- log10(1 + 1/seq(1:length(fd.degree)))
benford <- round(num.nodes * benford)

df <- data.frame(digit = seq(1:length(fd.degree)), 
                 first.digit.degree = fd.degree, 
                 first.digit.betweenness = fd.betweenness, 
                 first.digit.clustering = fd.clustering,
                 benford = benford, 
                 graph = 1)

plot <- ggplot(data = df, aes(x = digit, group = graph)) + 
  geom_line(aes(y = first.digit.degree.Freq, color = "degree")) + 
  geom_line(aes(y = first.digit.betweenness.Freq, color = "betweenness")) + 
  geom_line(aes(y = first.digit.clustering.Freq, color = "clustering coefficient")) +
  geom_line(aes(y = benford, color = "Benford")) +
  theme(legend.title=element_blank()) +
  scale_x_discrete(breaks = 1:9, labels = 1:9) +
  ylab("first digit count")




# compute the distribution of the alpha centrality for a fixed alpha = 0.5 and retain only non-zero values
a <- alpha.centrality(g, alpha = 1)
a <- a[a > 0]

# compute the distribution of degree first digit and compute the Pearson's correlation coefficient of this distribution to the Benford's distribution
fd.alpha <- table(sapply(a, first.digit))
benford <- log10(1 + 1/seq(1:length(fd.alpha)))
benford <- round(num.nodes * benford)
alpha.pcc <- cor(fd.alpha, benford)

test.alpha        <- benford(a, number.of.digits = 1, discrete = TRUE)

test.alpha.result <- c(data.file, num.nodes, num.edges, "alpha centrality", test.alpha$stats$chisq$statistic, test.alpha$stats$chisq$p.value, test.alpha$MAD, 
                       test.alpha$stats$mantissa.arc.test$statistic, test.alpha$stats$mantissa.arc.test$p.value, test.alpha$distortion.factor, alpha.pcc)
