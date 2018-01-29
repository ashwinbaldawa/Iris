##################################K-means Clustering on Iris Dataset################################

#Loading the required Libraries

library(ggplot2)
library(cowplot)

#Exploring the datasets

str(iris)
View(iris)

#Performing Exploratory Data Analysis on the dataset

plot_grid(
ggplot(iris,aes(x=Species,y=Sepal.Length)) + geom_bar(stat = "identity"),
ggplot(iris,aes(x=Species,y=Sepal.Width))  + geom_bar(stat = "identity"),
ggplot(iris,aes(x=Species,y=Petal.Length)) + geom_bar(stat = "identity"),
ggplot(iris,aes(x=Species,y=Petal.Width))  + geom_bar(stat = "identity"),align = "h"
)

ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+geom_point()

#Finding the mean of the Sepal Length across Species

aggregate(Sepal.Length~Species, data=iris ,mean)
aggregate(Sepal.Width~ Species, data=iris ,mean)
aggregate(Petal.Length~Species, data=iris ,mean)
aggregate(Petal.Width~ Species, data=iris ,mean)

#Finding if any outliers exist in the IRIS dataset

quantile(iris$Sepal.Length,seq(0,1,0.01))
quantile(iris$Sepal.Width,seq(0,1,0.01))
quantile(iris$Petal.Length,seq(0,1,0.01))
quantile(iris$Petal.Width,seq(0,1,0.01))

#Boxplot for all the datasets

plot_grid(
  ggplot(iris,aes(x=Species,y=Sepal.Length))+geom_boxplot(),
  ggplot(iris,aes(x=Species,y=Sepal.Width)) +geom_boxplot(),
  ggplot(iris,aes(x=Species,y=Petal.Length))+geom_boxplot(),
  ggplot(iris,aes(x=Species,y=Petal.Width)) +geom_boxplot(),align = "h"
  )

par(mfrow=c(2,2))
for(i in 1:4) boxplot(iris[,i]~Species,data=iris,main=names(iris)[i])

par(mfrow=c(1,1))
hist(iris$Petal.Length[1:50])

#Clustering the different Iris dataset using K-means by eliminating the Species column

irisCluster<-kmeans(iris[,1:4],3,nstart = 20)

#irisCluster$totss
#irisCluster$withinss
#irisCluster$tot.withinss
#irisCluster$betweenss

#Evaluating the k-means results by forming the results versus Species

table(iris$Species,irisCluster$cluster)

#Setosa seems to be correctly identified
#Versicolor species has 48 correct and 2 incorrect classifications
#Virginica  species has 36 correct and 14 incorrect classifications
#The reason being the set of predictor variables for Versicolor and virginica
#being very close and Setosa variables being very different from the other too


#Plot the resulting K-means

plot(iris[,c("Sepal.Length","Sepal.Width")],col=irisCluster$cluster)
points(irisCluster$centers[,c("Sepal.Length","Sepal.Width")],pch =8,cex=2)
