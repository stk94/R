library(MASS) #Library for dataset
library(ggplot2) #Visualization Library
library(NbClust) #For Clustering
library(factoextra) #For plotting clustering parameters using ggplot
library(fpc) #for density based clustering
library(dbscan) # for KNN distance plot

## Selecting the biopsy dataset
data(biopsy)

## Desctiption of Biopsy Dataset
?biopsy

## Setting seed for the project
set.seed(123456)

## Data Exploration
dim(biopsy) ## Dimension of dataset
str(biopsy) ## Structure of each column
summary(biopsy) ## Summary of each column in the dataset
table(biopsy$class) ## Distribution of the class label in the dataset

## Remove the rows containing NA Values
bp <- biopsy[complete.cases(biopsy),]

##### K MEANS CLUSTERING #####
?kmeans ## Description about kmeans clustering

## create 2 clusters using kmeans
c2 <- kmeans(bp[,2:10],2)
c2$size ## check the cluster size
c2
## confusion matrix between the new clusters and the class labels
table(bp$class, c2$cluster) 

## create 3 clusters using kmeans
c3 <- kmeans(bp[,2:10],3)
## confusion matrix between the new clusters and the class labels
table(bp$class, c3$cluster)

## create 4 clusters using kmeans
c4 <- kmeans(bp[,2:10],4)
table(bp$class,c4$cluster)

## create 5 clusters using kmeans
c5 <- kmeans(bp[,2:10],5)
table(bp$class,c5$cluster)

## create 6 clusters using kmeans
c6 <- kmeans(bp[,2:10],6)
table(bp$class,c6$cluster)

## Calculate the wthin sum of squares for the clusters 1 to 15
dt <- bp[,2:10]
dissim <- (nrow(dt)-1)*sum(apply(dt,2,var))
for (i in 2:15) dissim[i] <- sum(kmeans(dt,
                                        centers=i)$withinss)

dissim <- dissim/100

## Elbow plot - plot the within Sum of squares for clusters 1 to 15
plot(1:15, dissim, type="b", xlab="Number of Clusters",
     ylab="Within groups Dissimilarity",
     main="Elbow graph for optimal number of clusters",
     pch=20, cex=2)


##### HIERARCHICAL CLUSTERING #####
?hclust ## Description of Hierarchial clustering function

## Creating sample of 40 elements for visualizing
id <- sample(1:nrow(bp), 40)
bpSample <- bp[id,]

## Running Heirarchical Clustering on a sample data
hc <- hclust(dist(bpSample[,2:10]), method="ave")

## Plotting the hierarchial clustering
plot(hc, hang=-1, labels=bpSample$class, xlab="Dimensions", main="Hierarchicial Clustering Demonstration")
## splitting the plot with 2 clusters
rect.hclust(hc,k=2)

## Running Hierarchical clustering in complete data
hcFull <- hclust(dist(bp[,2:10]), method="ave")
bp$hc2 <- cutree(hcFull, k=2) ## Divide the hclust into 2 clusters
bp$hc3 <- cutree(hcFull, k=3) ## Divide the hclust into 3 clusters
## Confusion matrix between the new cluster and the defined class labels
table(bp$class, bp$hc2)  
table(bp$class, bp$hc3)

## Running KNN dist plot to findout optimal epsilon value for Density based clustering
dbscan::kNNdistplot(bp[,2:10], k=6)
abline(h=1.0, lty=2)

## Density based clustering. with epsilon 1.0 and MinPts 10
ds1 <- fpc::dbscan(bp[,2:10], eps=1.0, MinPts =10)
table(bp$class, ds1$cluster) ## Confusion Matrix

## Density based clustering with epsilon 0.1 and MinPts 24
ds2 <- fpc::dbscan(bp[,2:10], eps=0.1, MinPts =24)
table(bp$class, ds2$cluster) #Confusion Matrix
