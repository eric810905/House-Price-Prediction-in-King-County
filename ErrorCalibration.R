#load packages
library(dplyr)
library(stringr)
library(randomForest)
library(Metrics)
library(RColorBrewer)
library(flexclust)
library(beepr)

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


#Read and format data
houses<-read.csv('cleaned_data_zillow.csv')
testErrorAll = read.csv('testError5CV.csv', header = T)
choose.pc=c(9,11)
k=10
crossVlidationNumber=5
errorThreshold=0.1

numCluster=0
capture.ob<-data.frame()
capture.size=vector()
capture.mean=vector()
capture.var=vector()
train.cl.mean=vector()
train.cl.var=vector()
train.cl.size=vector()

n<-floor((1/crossVlidationNumber)*nrow(houses))
for(i in 1:crossVlidationNumber-1){
  cat("start: ", i)
  
  #get train and test
  testset<-houses[(n*(i-1)+1):(i*n), ]
  trainset<-houses[-((n*(i-1)+1):(i*n)), ]
  testError=getTestError(houses = trainset)
  write.csv(testError,file = paste("testErrorTainset10CV", i, ".csv", sep = ""))
  
  #also the residual result of train and test
  testErrorTestset<-testErrorAll[(n*(i-1)+1):(i*n), ]
  
  #apply pca on train
  pr.out=prcomp(trainset[,2:19], scale=TRUE)
  sales_pc=data.frame(pr.out$x)
  
  #kmeans on pc2 and pc17
  sales_use=sales_pc[,choose.pc]
  
  #run kmeans
  set.seed(1)
  cl1 = kcca(sales_use, k, kccaFamily("kmeans"))
  testError=cbind(testError, predict(cl1))
  
  #capture the clusters with high test error
  cluster.large.error=vector()
  for(j in 1:k){
    cluster.mean=abs(mean(testError$ErrorPercentage[testError$`predict(cl1)`==j]))
    if(cluster.mean>errorThreshold){
      train.cl.mean=c(train.cl.mean, cluster.mean)
      train.cl.var=c(train.cl.var, var(testError$ErrorPercentage[testError$`predict(cl1)`==j]))
      train.cl.size=c(train.cl.size, sum(testError$`predict(cl1)`==j))
      cluster.large.error <- c(cluster.large.error, j)
      cat(", get cluster", j, " in training set with average test error percentage higher than ", errorThreshold)
    }
  }
  #use the captured clusters to predict test data
  #test data PCA
  test.pc=predict(pr.out, testset[,2:19])
  test.pc=test.pc[,choose.pc]
  
  #predict cluster
  pred.cl <- predict(cl1, test.pc)
  testErrorTestset = cbind(testErrorTestset, pred.cl)
  for(j in cluster.large.error){
    select.cl=testErrorTestset[testErrorTestset$pred.cl==j,]
    if(nrow(select.cl)==0) {
      capture.mean=c(capture.mean, NA)
      capture.var=c(capture.var, NA)
      capture.size=c(capture.size, NA)
      next
    }
    numCluster=numCluster+1
    select.cl[,'pred.cl']=numCluster
    select.cl$origin.cl=j
    capture.mean=c(capture.mean, mean(select.cl$ErrorPercentage))
    capture.var=c(capture.var, var(select.cl$ErrorPercentage))
    capture.size=c(capture.size, nrow(select.cl))
    capture.ob=rbind(capture.ob, select.cl)
    
    hist(select.cl$ErrorPercentage)
    cat("Cluster ", (numCluster), " captured: size", (capture.size), ", mean test error percentage", capture.mean)
  }
  print("")
}

capture.size
capture.var
capture.mean
plot(capture.mean)
sum(capture.size)
for(i in 1:10){
  hist(capture.ob[capture.ob$pred.cl==i, "error.pct"])
}
hist(capture.ob[capture.ob$origin.cl==1, "error.pct"])
hist(capture.ob[capture.ob$origin.cl==4, "error.pct"])
hist(capture.ob[capture.ob$origin.cl==8, "error.pct"])
hist(capture.ob[capture.ob$origin.cl==23, "error.pct"])
abs(capture.mean)>0.2
cols<-brewer.pal(n=9,name="Set1")

plot(train.cl.mean, col=cols[2], ylim=c(-.6, 0.2), lwd=2, main='Average Test Error Percentage of Each Clusters and Predicted Cluster', 
     sub='The clusters are chosen from 10-Fold Cross Validation', 
     xlab = 'Clusters with High Error Percentage (>+/-10%)', ylab='Average Error Percentage(Test Error/Predictied Values)')
points(capture.mean, col=cols[1], pch=5, lwd=2)
legend(x=0.8,y=0.25,legend=paste(c("Clusters from Applying K-means (K=10) on Train Set","Clusters of the k-means Prediction on Test Set")),
       col=cols[c(2, 1)],bty="n", pch=c(1,5), cex=1.5, lwd=2)
