library(vegan)
#--------------------start-------------------------------
# Get current working directory
getwd()
#----------------read dataset----------------------------
data<-read.table("C:\\Users\\DauntlessD\\Documents\\~Personal\\Masters INRTU\\S2\\Data Analysis\\Practical class 5 Claster Analysis\\data.txt",header=TRUE,sep="\t",check.names = FALSE)
summary (data)
data
rownames(data)<-data[,1]
data<-data[,-1]
data
# Non-metric multidimensional scaling (NMDS) using Euclidean distances
# Perform NMDS ordination based on Euclidean distances.
# Euclidean distance is suitable for quantitative data but may be less appropriate
# for ecological community data due to sensitivity to double zeros.
# The metaMDS function automatically runs multiple random starts and scales the solution.
ord <- metaMDS(data, distance = "euclidean")

#Visualisation of NMDS results
plot(ord, type = "n")                     # Create empty plot (axes only)
points(ord, disp = "sites", pch = 21, 
       cex = 2.5, lwd = 2.5, col = "red") # Add site points (samples)
text(ord, display = "site", cex = 0.7, 
     col = "red", pos = 3)                         # Label sites with their names


# Cluster analysis with Euclidean distances
# Compute Euclidean distance matrix between sites
d<-vegdist(data,method="euclidean")
# Perform hierarchical agglomerative clustering using average linkage (UPGMA)
fit<-hclust(d, method="average")

# Visualise the dendrogram with labels aligned at the baseline
plot(fit, hang =-1)
# Simple dendrogram plot (default parameters)
plot(fit)


#/////////////////////////////////////////////////////////////
#Non-metric multidimensional scaling (NMDS) with Bray–Curtis dissimilarities
# Bray–Curtis is a standard ecological distance measure that ignores double zeros
# and is robust for abundance data.
# The metaMDS function automatically runs multiple random starts and scales the final solution.
ord <- metaMDS(data, distance = "bray")

#Visualisation of NMDS results
plot(ord, type = "n")
points(ord, disp="sites", pch=21, cex=2.5, lwd=2.5, col = "red")
text(ord, display = "site", cex=0.7, col="red", pos = 3)

# Cluster analysis with Bray–Curtis distances
# Compute Bray–Curtis dissimilarity matrix (standard for ecological community data)
d<-vegdist(data,method="bray")
# Perform hierarchical agglomerative clustering using average linkage (UPGMA)
fit<-hclust(d, method="average")

# Visualise dendrogram with labels aligned at the same horizontal level
plot(fit, hang =-1)
# Simple dendrogram plot (default R style)
plot(fit)


#/////////////////////////////////////////////////////////////
# Non-metric multidimensional scaling (NMDS) with Jaccard distance
ord <- metaMDS(data, distance = "jaccard")


plot(ord, type = "n")
points(ord, disp="sites", pch=21, cex=2.5, lwd=2.5, col = "red")
text(ord, display = "site", cex=0.7, col="red", pos = 3)

# Cluster analysis with Jaccard distance
# Compute Jaccard dissimilarity matrix
d<-vegdist(data,method="jaccard")
# Hierarchical clustering using average linkage (UPGMA)
fit<-hclust(d, method="average")
# Visualise dendrogram with labels aligned
plot(fit, hang =-1)
# Default dendrogram plot
plot(fit)
# ==============================================================================
# PERMANOVA ANALYSIS (Bray-Curtis, 999 Permutations)
# ==============================================================================

# 1. Choose the number of clusters (k) by cutting the Bray-Curtis dendrogram.
# change k = 3 to whatever number of clusters your practical assignment requires.
my_clusters <- cutree(fit, k = 3) 

# 2. Run the PERMANOVA
# Note: 'data' contains your species abundance matrix. 
# We map 'data' against the 'my_clusters' vector we just created.
permanova_result <- adonis2(data ~ my_clusters, 
                            method = "bray", 
                            permutations = 999)

# 3. Print the results to the console
print(permanova_result)