##
##
## 02 - Multivariate Exploration
##
##

library (cclust)
library(factoextra)
library(FactoMineR)

# Color palette
colors <- sample(c('#000000', '#FF0000', '#808000', '#00FF00', '#008000', 
                   '#00FFFF', '#0000FF', '#FF00FF', '#800080', '#ffa500'))


####################  Load dataset #########################
data <- read.csv("training.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)

# set variables as factors
as.factor(data$Region)


################### PCA ##########################


pca.Happiness = PCA(data,scale.unit = TRUE,quali.sup = 1,graph=FALSE)
plot(pca.Happiness, cex=0.8, choix="var")

# Let's see how the groups are distributed in the factorial space
plotInd <- fviz_pca_ind(pca.Happiness,
                        label = "none", # hide individual labels
                        habillage = data$Region, # color by region
                        palette = colors,
                        invisible = "ind.sup",
                        addEllipses = FALSE # Concentration ellipses
)
ggpubr::ggpar(plotInd ,
              legend = c(0.2, 0.3)
              
)

# using elbow and raiser rule we select 5 components.
plot(pca.Happiness$eig[,1],type="b",col='blue',ylab="Eigenvalue",xlab="Component Number")
abline(h=1,lty=2,col="red")

################## Clustering  ####################

# # first, perform a hierarchical clustering

# # if we perform on clustering on pca, get factorial coordinates of individuals
# df.pca <- res.pca$ind$coord[, 1:signi]

# hierarchical clustering on factorial coordinates from PCA
# calculate a distance matrix between individuals which uses the squared

# # then we apply hclust with method="ward"
# hward = hclust(distances, method="ward.D2")

# # plot dendrograms
# plot(hward)

# # try number of clusters
# cl <- ...
# # cut tree and see the size of clusters
# c1 <- cutree(hward, cl)
# table(c1)
# # Calculate centroids of clusters
# centroids <- df.pca[1:cl,]
# row.names(centroids) <- 1:cl
# for (i in 1:cl) {
#     for (j in 1:ncol(centroids)) {
#         centroids[i,j] <- mean(df.pca[which(c1 == i),j])
#     }
# }

# # Plot points in first factorial axis with centroids
# plot(df.pca[,1], df.pca[,2], col=colors[c1],
#      main="Individuals by cluster group in first factorial plane",
#      xlab="PC1", ylab="PC2", cex.lab=0.8)
# plotAddSubtitle(subtitle)
# abline(v = 0, h = 0, lty = 2)
# points(centroids, bg = colors[1:cl], cex=2,pch=21, col=colors[1:cl])

# # Consolidate clusters by doing k-means
# cl.kmeans <- kmeans(df.pca, centroids)
# clust <- cl.kmeans$cluster

# # Plot points in first factorial axis with centroids
# plot(df.pca[,1], df.pca[,2], col=colors[clust],
#      main="Individuals by cluster group in first factorial plane
#      \nConsolidated clusters",
#      xlab="PC1", ylab="PC2", cex.lab=0.8)
# plotAddSubtitle(subtitle)
# abline(v = 0, h = 0, lty = 2)
# points(centroids, bg = colors[1:cl], cex=2,pch=21, col=colors[1:cl])

# # Calinski Harabassz index 
# 
# ch <- rep(0, 9)
# for (i in 2:10) {
#     c2 <- cutree(hward, i)
#     ch[i-1] <- calinhara(df.pca, c2)
# }
# 
# plot(2:10, ch, type = "b", main="Calinski-Harabassz Index vs number of 
#      clusters",
#      xlab="Number of clusters", ylab="C-H Index", cex.lab=0.8)
# plotAddSubtitle(subtitle)
