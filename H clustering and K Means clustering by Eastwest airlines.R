library("dummies")
eastwestairlines=read.csv("eastwestairlines.csv",header = TRUE)
View(eastwestairlines)
attach(eastwestairlines)
##  Loading and preparing data
mydatawd  <- eastwestairlines[,2:11]
View(mydatawd)

#Creating Dummy variables for categorical data
mydata    <- dummy.data.frame(mydatawd, names = "cc1_miles", omit.constants=FALSE )
mydata    <- dummy.data.frame(mydatawd, names = "cc2_miles", omit.constants=FALSE )
mydata    <- dummy.data.frame(mydatawd, names = "cc3_miles", omit.constants=FALSE )
# After Creating dummy variable see the data set 
View(mydata)

# Standardize Data
my_data   <- scale(mydata)

# 2. Compute dissimilarity matrix
d=dist(my_data, method = "euclidean")

# Hierarchical clustering using complete method
fit<- hclust(d, method = "complete" )
fit

plot(fit) # display dendrogram
plot(fit, hang=-1)# Here hang =-1 in order to arrange clustering numbers in single sequence line
groups <- cutree(fit, k=5)# cut tree into 5 clusters
rect.hclust(fit, k=5, border="red") ## Cluster line represent in red line for better visualization
## hence its very diificult to say in cluster how many categories because it is big data 
## so we go to the K Means Clustering 


######### Kmeans clustering ####
fit <- kmeans(my_data, 5) # 4 cluster solution
str(fit) ## Actually withinness sum of square should be less value 
## Betweenness sum of square should have high value but we  got reverse 
install.packages("animation")
library(animation)
km <- kmeans.ani(my_data, 4)

final2<- data.frame(my_data, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate(my_data[,1:6], by=list(fit$cluster), FUN=mean)


# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xcl <- clara(my_data, 2, sample = 100)
clusplot(xcl)

##CONCLUSION:
## From above metrics, none of the clusters are matching, Hence the outcomes of K-means cluster and Hierarchial clustering are not same.
