#load packages
library(dplyr)
library(stringr)
library(randomForest)
library(Metrics)
library(RColorBrewer)
library(flexclust)


#Read and format data
houses<-read.csv('cleaned_data_zillow.csv')
#Convert yr_built and yr_renovated to duration
#houses$yr_built<-2015-houses$yr_built
#houses$yr_renovated[houses$yr_renovated==0]<-houses$yr_built[houses$yr_renovated==0]
#houses$yr_renovated[houses$yr_renovated>1000]<-2015-houses$yr_renovated[houses$yr_renovated>1000]
#Drop "ID" and "Date"
#houses<-houses[,!colnames(houses) %in% c("id","date")]
#summary(houses)
#sapply(houses, class)

getTestError=function(houses){
  #cross-validation
  bag.mse<-vector()
  bag.rsq<-vector()
  results<-data.frame()
  overall<-data.frame()
  0.1*nrow(houses)
  n<-floor(0.1*nrow(houses))
  for(i in 1:9){
    testset<-houses[(n*(i-1)+1):(i*n), ]
    trainset<-houses[-((n*(i-1)+1):(i*n)), ]
    bag.houses<-randomForest(price~.,data = trainset, mtry=9,
                             importance=TRUE, ntree=200)
    houses.pred<-predict(bag.houses,newdata=testset)
    bag.mse[i]<-bag.houses$mse[200]
    bag.rsq[i]<-bag.houses$rsq[200]
    results<-cbind(testset$price, houses.pred, houses.pred-testset$price)
    overall<-rbind(overall, results)
  }
  testset<-houses[(9*n+1):nrow(houses),]
  trainset<-houses[1:9*n,]
  bag.houses<-randomForest(price~.,data = trainset, mtry=9,
                           importance=TRUE, ntree=200)
  houses.pred<-predict(bag.houses,newdata=testset)
  bag.mse[10]<-bag.houses$mse[200]
  bag.rsq[10]<-bag.houses$rsq[200]
  results<-cbind(testset$price, houses.pred, houses.pred-testset$price)
  overall<-rbind(overall, results)
  
  colnames(overall) = c("TruePrice", "PredictPrice", "TestError")
  overall$ErrorPercentage=overall$TestError/overall$PredictPrice
  return(overall)
}


number.cl=1
capture.ob<-data.frame()
capture.size=vector()
capture.mean=vector()
capture.var=vector()
train.cl.mean=vector()
train.cl.var=vector()
train.cl.size=vector()
clusters.all=data.frame()

n<-floor(0.1*nrow(houses))
residual = read.csv('residual.csv', header = T)
choose.pc=c(9,11)
k=10

0.1*nrow(houses)
n<-floor(0.2*nrow(houses))
for(i in 1:1){
  testset<-houses[(n*(i-1)+1):(i*n), ]
  trainset<-houses[-((n*(i-1)+1):(i*n)), ]
  testError=getTestError(houses = trainset)
}

i=1 #get train and test
testset<-houses[(n*(i-1)+1):(i*n), ]
trainset<-houses[-((n*(i-1)+1):(i*n)), ]
testError=getTestError(houses = trainset)

#also the residual result of train and test
test.residual<-residual[(n*(i-1)+1):(i*n), ]
train.residual<-residual[-((n*(i-1)+1):(i*n)), ]

#apply pca on train
pr.out=prcomp(trainset[,2:19], scale=TRUE)
sales_pc=data.frame(pr.out$x)

#kmeans on pc2 and pc17
sales_use=sales_pc[,choose.pc]

#run kmeans
set.seed(1)
cl1 = kcca(sales_use, k, kccaFamily("kmeans"))
testError=cbind(testError, predict(cl1))
colnames(cluster)[4] = "km.out$cluster"
cluster$error.pct=cluster$Residual/cluster$Prediction

#capture the clusters with high test error
cluster.large.error=vector()
for(j in 1:k){
  cluster.mean=abs(mean(testError$ErrorPercentage[testError$`km.out$cluster`==i]))
  if(cluster.mean>0.1){
    train.cl.mean=c(train.cl.mean, cluster.mean)
    train.cl.var=c(train.cl.var, var(cluster$error.pct[cluster$`km.out$cluster`==j]))
    train.cl.size=c(train.cl.size, sum(cluster$`km.out$cluster`==j))
    cluster.large.error <- c(cluster.large.error, j)
    cat(", get cluster", j)
  }
}

# calculate the mean, variance of the error percentage in each cluster, and the size of each cluster
cluster.mean=0
cluster.var=0
cluster.size=0
for(i in 1:k){
  cluster.mean[i]=abs(mean(testError$ErrorPercentage[testError$`km.out$cluster`==i]))
  cluster.var[i]=var(testError$ErrorPercentage[testError$`km.out$cluster`==i])
  cluster.size[i]=sum(testError$`km.out$cluster`==i)
  
  # plot the histogram of the cluster if the mean of error percentage is larger than 10%
  if(cluster.mean[i]>0.1)  hist(testError$ErrorPercentage[cluster$`km.out$cluster`==i])
}

# clusters info
cluster.anlysis=data.frame(cbind(1:k, cluster.mean, cluster.var))

# plot cluster info
cols = brewer.pal(n=8,name="Set1")
plot(cluster.mean, col=cols[1], pch=5, lwd=2, main='Absolute Average Test Error Percentage of Each Clusters', 
     sub='The clusters are from K-means Clustering(K=10) on PC9 and PC11', 
     xlab = 'Cluster', ylab='Absolute Average Error Percentage(Test Error/Predictied Values)')
points(cluster.var, col=cols[2])
cluster.size

hist(cluster$error.pct[cluster$`km.out$cluster`==11])
hist(cluster$error.pct[cluster$`km.out$cluster`==15])

#k=20 cluster=11
#k=15 cluster=4
#PC15-PC19 k=30
#PC15-PC19 k=15
#PC2+PC17!!!!

#show cluster on PC2 and PC17
plot(sales_pc[,c(2,17)], main='4 Clusters with High Test Error Percentage (>20%) on PC2 and PC17', sub='The clusters are from K-means Clustering(K=30) on PC2 and PC17')
plot.select=sales_pc[(cluster$`km.out$cluster`==c(14, 20, 23, 25)),c(2,17)]
plot.cols=cluster$`km.out$cluster`[cluster$`km.out$cluster`==c(14, 20, 23, 25)]
points(sales_pc[(cluster$`km.out$cluster`==c(14, 20, 23, 25)),c(2,17)], col=plot.cols, pch=1, cex=2,lwd=2)
legend(x=-4,y=8,legend=paste(c("cluster 14","cluster 20","cluster 23","cluster 25")),col=c(14, 20, 23, 25),bty="n", pch=1, cex=1.5 )


#show cluster on residual
#when k=30
plot(cluster$Prediction, cluster$error.pct, main='4 Clusters with High Test Error Percentage (>20%) on Error Percentage Plot', 
     sub='The clusters are from K-means Clustering(K=30) on PC2 and PC17', xlab = 'Predicted Values', ylab='Error Percentage(Test Error/Predictied Values)')
plot.cols=cluster$`km.out$cluster`[cluster$`km.out$cluster`==c(14, 20, 23, 25)]
points(cluster[(cluster$`km.out$cluster`==c(14, 20, 23, 25)),c('Prediction', 'error.pct')], col=plot.cols, pch=1, cex=2,lwd=2)
legend(x=2300000,y=-0.8,legend=paste(c("cluster 14: -26% average test error","cluster 20: -28% average test error","cluster 23: -36% average test error","cluster 25: -34% average test error")),col=c(14, 20, 23, 25),bty="n", pch=1, cex=1.5 )
