##
##
## 02 - Multivariate Exploration
##
##

library (cclust)
library(factoextra)
library(FactoMineR)
library(fpc)

# Color palette
colors <- sample(c('#000000', '#FF0000', '#808000', '#00FF00', '#008000', 
                   '#00FFFF', '#0000FF', '#FF00FF', '#800080', '#ffa500'))


####################  Load dataset #########################
data <- read.csv("training.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)
sup <- read.csv("test.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)

# set variables as factors
as.factor(data$Region)


################### PCA ##########################
df.pca <- rbind(data, sup)

pca.Happiness = PCA(df.pca, scale.unit = TRUE, ind.sup=(nrow(data)+1):nrow(df.pca), quali.sup = 1,graph=FALSE)
fviz_pca_var(pca.Happiness,  col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE)

# Let's see how the groups are distributed in the factorial space
plotInd <- fviz_pca_ind(pca.Happiness,
                        label = "all", # hide individual labels
                        habillage = data$Region, # color by region
                        palette = colors,
                        invisible = "ind.sup",
                        addEllipses = FALSE, # Concentration ellipses
                        show.legend = FALSE
)
plotInd <- fviz_add(plotInd, pca.Happiness$ind.sup$coord[,1:2],
         addlabel=T, color=alpha("#E05C00",0.4), shape=20, pointsize=4)
ggpubr::ggpar(plotInd ,
              legend = c(0.1, 0.2),
              show.legend = FALSE
              
)

# using elbow and raiser rule we select 5 components.
plot(pca.Happiness$eig[,1],type="b",col='blue',ylab="Eigenvalue",xlab="Component Number", main= "Cumulative Variance Explained of Eigen Values")
abline(h=1,lty=2,col="red")
text(pca.Happiness$eig[,1], labels = round(pca.Happiness$eig[,3], digits = 2), col = 4, pos = 1)

################## Clustering  ####################

## first, perform a hierarchical clustering
# significant dimensions
signi <- 5
# if we perform on clustering on pca, get factorial coordinates of individuals
df.pca <- pca.Happiness$ind$coord[, 1:signi]

# hierarchical clustering on factorial coordinates from PCA
# calculate a distance matrix between individuals which uses the squared
distances = dist(df.pca, method = "euclidean")
## then we apply hclust with method="ward"
hward = hclust(distances, method="ward.D2")

# plot dendrograms
plot(hward)

########## --------- Declare functions --------- ##########

clusterData <- function(data, distance.method="euclidean",
                        cl.method="ward.D2",
                        min.cl = 2, max.cl = 10, consolidation=FALSE) {
    
    #Control parameters
    if (min.cl > max.cl) {
        print("Error in cluster min:max interval")
        return()
    }
    
    # Calculate a distance matrix between individuals 
    # which uses the squared Euclidean distance
    distances <- dist(data, method=distance.method)
    
    # Perform hierarchical clustering
    hward <- hclust(distances, method=cl.method)
    
    # Initialize storing variables
    ch <- rep(0, max.cl - min.cl + 1)
    # Loop for number of clusters cl
    for (m in 1:(max.cl - min.cl + 1)) {
        cl <- min.cl + m - 1
        
        #print(paste("Number of clusters:", cl))
        
        # cut tree and see the size of clusters
        clust <- cutree(hward, cl)
        #print(table(clust))
        
        if (consolidation) {
            
            # Calculate centroids of clusters
            centroid <- data[1:cl,]
            row.names(centroid) <- 1:cl
            for (i in 1:cl) {
                for (j in 1:ncol(centroid)) {
                    centroid[i,j] <- mean(data[which(clust == i),j])
                }
            }
            
            # Perform consolidation
            cl.kmeans <- kmeans(data, centroid)
            clust <- cl.kmeans$cluster
            
        }
        
        # Calculate Caliniski-Harabazi index for clustering
        ch[m] <- calinhara(data, clust)
        #print(paste("Calinhara index:", ch[m]))
        
    }
    return(ch)
    
}

plotAddSubtitle <- function(subtitle) {
  par(adj = 1)    # Align subtitle to the right
  title(sub = subtitle, cex.sub=1.1)
  par(adj = 0.5)  # Reset alignment
}

########## ------- End Declare functions -------- ##########

method <- "Ward"
subtitle <- paste("Aggregation method:", method)

## Plot calinski without consolidation.
min.cl <- 2
max.cl <- 15
noConsol <- clusterData(df.pca, min.cl = min.cl, max.cl = max.cl, 
                        consolidation = FALSE)
subtitle2 <- paste(subtitle, "Consolidation: No", sep = ", ")
plot(min.cl:max.cl, noConsol, type = "b",
     main="Calinski-Harabasz Index vs number of clusters",
     xlab="Number of clusters", ylab="C-H Index", cex.lab=0.8)
plotAddSubtitle(subtitle2)
## Plot calinski with consolidation.
yesConsol <- clusterData(df.pca, min.cl = 2, max.cl = 15, 
                         consolidation = TRUE)
subtitle3 <- paste(subtitle, "Consolidation: Yes", sep = ", ")
plot(min.cl:max.cl, yesConsol, type = "b", 
     main="Calinski-Harabasz Index vs number of clusters
     \nConsolidated clusters",
     xlab="Number of clusters", ylab="C-H Index", cex.lab=0.8)
plotAddSubtitle(subtitle3)

# Chosen number of clusters
cl <- 4
# cut tree and see the size of clusters
clust <- cutree(hward, cl)
# Calculate centroids of clusters
centroid <- df.pca[1:cl,]
row.names(centroid) <- 1:cl
for (i in 1:cl) {
  for (j in 1:ncol(centroid)) {
    centroid[i,j] <- mean(df.pca[which(clust == i),j])
  }
}
# Perform consolidation
cl.kmeans <- kmeans(df.pca, centroid)
clust <- cl.kmeans$cluster
table(clust)

# Plot dendrogram with color
fviz_dend(hward, cex = 0.5, k = cl, ggtheme = theme_minimal(),repel=T) 


# visualize clusters using the first two factorial coordinates
plotInd <- fviz_pca_ind( pca.Happiness, 
             label="all",
             habillage =  as.factor(clust), #color by cluster
             addEllipses=FALSE,
             invisible = "ind.sup",
             ggtheme = theme_minimal(),
             show.legend = FALSE,
             repel =T
)

ggpubr::ggpar(plotInd ,
              legend = c(0.1, 0.2),
              show.legend = FALSE
              
)

######### Profiling ######### 
df.prof <- data
df.prof$cl <- as.factor(clust)
catdes(df.prof, num.var = ncol(df.prof))
