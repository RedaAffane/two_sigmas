library(cba)

corr_binary = read.csv("./corr_binary.csv")
rownames(corr_binary) = corr_binary$X
corr_binary$X = NULL
str(corr_binary)

humming_dist=dist(corr_binary,method = "binary")
str(humming_dist)
clusters = hclust(humming_dist,method = "complete")
plot(clusters)
summary(clusters)
clusterCut <- cutree(clusters, 100)
table(clusterCut)
