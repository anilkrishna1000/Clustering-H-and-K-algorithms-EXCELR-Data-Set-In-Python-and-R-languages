crimedata=read.csv("crimedata")
data1=crimedata[1:50,c(2:5)]# i want all the rows and columns from 2 to 5 so only this line 
View(data1)


normalized_data <- scale(data1) #excluding the country name in 1 st column name and normalizing the data
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
fit

plot(fit) # display dendrogram
plot(fit, hang=-1)# Here hang =-1 in order to arrange clustering numbers in single sequence line
groups <- cutree(fit, k=5)# cut tree into 5 clusters

?cutree
rect.hclust(fit, k=5, border="red")
?rect.hclust

membership<-as.matrix(groups)


final <- data.frame(data1, membership)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))] # this line code means that in view data set membership column comes first column
View(final1)


