library(ridge)
library(MASS)
library(caret)
library(caTools)
main<-Boston[sample(nrow(Boston)),]
MSE<-NULL
bestmodel<-NULL
bestlambda<-NULL
MSE<-NULL
ridregmodel<-NULL
meanmse<-NULL
summary<-NULL
meanmsesummary<-NULL



CV.RidgeRegression <- function(y,X,K,repetations,lambdas)
{
  write.table(y,file = "y data",col.names = T,row.names = F)
  write(as.matrix(X),file = "x data",ncolumns = ncol(X))
  
  
  index<-createFolds(y, K, list = F, returnTrain = F)
  newmain <- cbind(index,main)
  for (i in 1:repetations)
  {
    for (j in 1:length(lambdas))
    {
      for(k in 1:K)
      {
        testset <- newmain[newmain$index %in% K,1:14] 
        trainset <- newmain[!(newmain$index %in% K),]
        ridregmodel[[k]] <- linearRidge(medv ~ ., data=trainset, lambda=lambdas[j]) 
        MSE[k] <- mean((testset - predict(ridregmodel[[k]],testset)) ^ 2)/nrow(trainset)
        a<-(paste("Run " , i ," Fold " , k , " MSE " , MSE[k] , " lambda " , j))
        summary<-append(summary,a)
      }
      meanmse[j] <- mean(MSE)
      s<-(paste("Mean MSE:",round(mean(MSE),digits = 4) ,"for lambda:",j))
      meanmsesummary<-append(meanmsesummary,s)
      bestmodel[j] <- ridregmodel[which.min(MSE)]
    }
  }
  finallambda<-lambdas[which.min(meanmse)]
  
  bml<-bestmodel[which.min(meanmse)]
  yhat <- predict(bml,X)
  plot(y,yhat[[1]],xlab='Observed Median Value',ylab='Prediction',
       main=paste('Ridge Regression with lambda =',finallambda))
  abline(coef=c(1,1))
  legend('topleft',c('Model vs obs','Ideal model output'),lty=c(NA,1),pch=c(1,NA))
  retli<-list(MeanMSE=meanmse,Summary=summary,MeanMSESummary=meanmsesummary,BestModelLi=bestmodel,lambdali = lambdas,SelectedModel = bml,SelectedLambda=finallambda)
  return(retli)
}
cd<-CV.RidgeRegression(main$medv,main[,1:13],10,2,c(1:3))
cd$Summary
cd$MeanMSE
cd$MeanMSESummary
cd$BestModelLi
cd$SelectedModel
cd$SelectedLambda
write(cd$Summary,file = "Summary Data")
cvresults <- CV.RidgeRegression(Boston$medv,Boston[setdiff(names(Boston),'medv')],
                                K=10,repetations=2,lambdas=seq(0,2,1))
