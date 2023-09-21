install.packages("scatterplot3d")
install.packages("outliers")
install.packages("spatstat")
install.packages(c("zoo","xts","quantmod"))
install.packages("remotes")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggrepel")
install.packages("dbscan")
library(scatterplot3d)
library(outliers)
library(spatstat)
library(DMwR)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggrepel)
library(dbscan)
library(tidyverse)

str(data)

data <- read.csv("SelectedVariables1.csv", header=TRUE)
data$Y <- as.factor(data$Y)
data$tourney_name <- as.factor(data$tourney_name)
data$surface <- as.factor(data$surface)


## Proximity based approach #####
knn_dist <- nndist(data[, -9], k = 1:10)
knn_dist

data$knn_anom_score <- apply(knn_dist,1,mean)
hist(data$knn_anom_score,xlab="k-Nearest Neighbors Average Distance")

df8=data.frame(data)

g6=ggplot()+
  geom_histogram(data=df8,aes(x=knn_anom_score), color="grey75", binwidth=0.1)
g6
boxplot(data$knn_anom_score,ylab="k-Nearest Neighbors Average Distance")
data[data$knn_anom_score>0.5,]




data2<-data
data2$orig_rows<-1:nrow(data)



res<-0
ids_M <- NULL

while(res<0.05){
  res<-grubbs.test(data2$X1stWon)$p.value
  id<-which.max(data2$X1stWon)
  
  
  ids_M<-c(ids_M,data2$orig_rows[id])
  data2<-data2[-id,]
}

data[ids_M[1:5],]

res<-0
ids_M<-NULL

while(res<0.05){
  res<-grubbs.test(data2$X2ndWon)$p.value
  id<-which.max(data2$X2ndWon)
  
  
  ids_M<-c(ids_M,data2$orig_rows[id])
  data2<-data2[-id,]
}

data[ids_M[1:5],]

x <- as.matrix(data$ace)
lof_scores_ace <- lof(x)
threshold <- mean(data$ace) + (3 * sd(data$ace))
anomalies <- which(lof_scores_ace > threshold)
plot(x, col = ifelse(lof_scores_ace > threshold, "red", "black"), pch = 16)
legend("topright", legend = c("Normal", "Anomaly"), col = c("black", "red"), pch = 16)
anomaly_rows <- data[anomalies, ]
anomaly_rows

y <- as.matrix(data$bpSaved)
lof_scores_bpSaved <- lof(y)
threshold <- mean(data$bpSaved) + (3 * sd(data$bpSaved))
anomalies <- which(lof_scores_bpSaved > threshold)
plot(y, col = ifelse(lof_scores_bpSaved > threshold, "red", "black"), pch = 16)
legend("topright", legend = c("Normal", "Anomaly"), col = c("black", "red"), pch = 16)
anomaly_rows <- data[anomalies, ]
anomaly_rows

z <- as.matrix(data$df)
lof_scores_df <- lof(z)
threshold <- mean(data$df) + (3 * sd(data$df))
anomalies <- which(lof_scores_df > threshold)
plot(z, col = ifelse(lof_scores_df > threshold, "red", "black"), pch = 16)
legend("topright", legend = c("Normal", "Anomaly"), col = c("black", "red"), pch = 16)
anomaly_rows <- data[anomalies, ]
anomaly_rows

