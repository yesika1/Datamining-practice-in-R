# K-means Clustering
#=============

# unsupervised learning algorithm: no labels, not training and test data
# divide data in groups (clusters) with obs. with similar behavior
# market segmentation
# Remember that Kmeans is an unsupervised learning algorithm, 
# meaning it doesn't really make sense to do a train test split because you would in reality have no labels to test against 

# 1. choose the k= number of cluster using elbow method
# 2. randomly assign points to clusters
# 3. for each cluster compute cluster centroid= mean vector of points in cluster
# 4. Assign data to closest clusters 
# 5. Repeat 3&4 until stable clusters

# elbow method: choose k at which SSE decreases abruptally
# compute SSE (Sum of Squared Errors) for some values of k(2,4,6)
# SSE: sum of the squared dist. between each point in cluster and the centroid
# plot k vs. SSE: error decrease as k is larger (smaller distorsion)


library(ISLR)
print(head(iris))
library(ggplot2)
iris <-iris
pl1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) +
  geom_point(size=2 )
# setosa will be easier to group


set.seed(101)
irisCluster <- kmeans(iris[,1:4],centers=3, nstart= 20)
print(irisCluster)
iris$clusters <- irisCluster$cluster

table(irisCluster$cluster, iris$Species)

library(cluster)
clusplot(iris,irisCluster$cluster, color = T, shade = T, labels = 0, lines = 0)


# Exercisestr
df_red <- read.csv("~Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-red.csv", sep=";")
df_white  <-read.csv("~/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-white.csv", sep=";")

# Adding column to both df indicating a label 'red' or 'white'.
df_red$label <- sapply(df_red$pH,function(x){'red'})
df_white$label <- sapply(df_white$pH,function(x){'white'})

#Combine df into a single data frame called wine.
wine <- rbind(df_red,df_white)
str(wine)


#EDA
library(ggplot2)

#Histogram of residual sugar from the wine data. Color by red and white wines
pl <- ggplot(wine,aes(x=residual.sugar)) + 
  geom_histogram(aes(fill=label),color='black',bins=50, position=position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c('#ae4554','#faf7ea')) + 
  theme_bw()

table (wine$residual.sugar,wine$label)
# red wine has lower residual sugar counts

#Histogram of citric.acid. Color by red and white wines.
pl2 <- ggplot(wine,aes(x=citric.acid)) + 
  geom_histogram(aes(fill=label),color='black',bins=50, position=position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c('#ae4554','#faf7ea')) + 
  theme_bw()

#Histogram of alcohol. Color by red and white wines.
pl3 <- ggplot(wine,aes(x=alcohol)) + 
  geom_histogram(aes(fill=label),color='black',bins=50, position=position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c('#ae4554','#faf7ea')) + 
  theme_bw()

#scatterplot of residual.sugar versus citric.acid, color by red and white wine.
pl4 <- ggplot(wine,aes(x=citric.acid,y=residual.sugar)) + 
  geom_point(aes(color=label),alpha=0.2) + 
  scale_color_manual(values = c('#ae4554','#ffffcc')) +
  theme_bw()

#scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.
pl5 <- ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) + 
  geom_point(aes(color=label),alpha=0.2) + 
  scale_color_manual(values = c('#ae4554','#ffffcc')) +
  theme_bw()

#model
#------

#  clus_data: wine data without the label and call it
clus_data <-wine[,1:12]

wine_clusterModel <- kmeans(clus_data,centers=2) #expect 2 types of wines

print(wine_clusterModel$centers)
#center values for each feature in each cluster (multidimensional data)

#evaluation
table(wine$label,wine_clusterModel$cluster)
wine$cluster <- wine_clusterModel$cluster
