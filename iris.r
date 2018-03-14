library(datasets)
library(caret)

setwd("D:\\Media\\Documents\\Abi\\Data Science\\Repositories")

irim<-iris

set.seed(1)
index<-sample(1:nrow(irim),0.70*nrow(irim),replace = F)
iristr<-irim[index,]
iriste<-irim[-index,]

#dtx_tr
#dtx_tr_uv_numeric
#dtx_tr_uv_numeric_histogram
par(mfrow=c(2,2))
for(i in 1:4){
hist(iristr[,i],xlab = names(iristr)[i])
}
?hist()
#dtx_tr_uv_numeric_density plot
par(mfrow=c(2,2))
for(i in 1:4){
  plot(density(iristr[,i]),xlab=names(iristr)[i])
}
#dtx_tr_uv_numeric_boxplot
par(mfrow=c(2,2))
for(i in 1:4){
  boxplot(iristr[,i],main=names(iristr)[i])
}
dev.off()
#dtx_tr_uv_cat_barplot
barplot(summary(iristr$Species))
summary(iristr$Species)

#dtx_tr_bi_(cont-cont)
pairs(iristr[,-5])
cor.test(iristr[,1],iristr[,2])
cor.test(iristr[,1],iristr[,3])
cor.test(iristr[,1],iristr[,4])
cor.test(iristr[,2],iristr[,3])
cor.test(iristr[,2],iristr[,4])
cor.test(iristr[,3],iristr[,4])


#dtx_tr_bi_(Cont_cat)
tapply(iristr$Sepal.Length,iristr$Species,mean)
t<-aov(Sepal.Length~Species,data = iristr)
summary(t)
tapply(iristr$Sepal.Width,iristr$Species,mean)
t<-aov(Sepal.Width~Species,data = iristr)
summary(t)
tapply(iristr$Petal.Length,iristr$Species,mean)
t<-aov(Petal.Length~Species,data = iristr)
summary(t)
tapply(iristr$Petal.Width,iristr$Species,mean)
t<-aov(Petal.Width~Species,data = iristr)
summary(t)

#Spot check
#List of models that can be used

#LDA, CART, KNN, SVM, rf
set.seed(7)
trct<-trainControl(method = "cv",number = 10, verboseIter = T)
fit.LDA<-train(Species~.,data = iristr,trControl=trct,metric="Accuracy",method="lda")
fit.CART<-train(Species~.,data = iristr,trControl=trct,metric="Accuracy",method="rpart")
fit.KNN<-train(Species~.,data = iristr,trControl=trct,metric="Accuracy",method="knn")
fit.SVM<-train(Species~.,data = iristr,trControl=trct,metric="Accuracy",method="svmLinear")
fit.rf<-train(Species~.,data = iristr,trControl=trct,metric="Accuracy",method="rf")

results<-resamples(list(LDA=fit.LDA,CART=fit.CART,KNN=fit.KNN,SVM=fit.SVM,RF=fit.rf))
summary(results)

#applying the LDA model in test data set

predtest<-predict(fit.LDA,iriste[,1:4])
confusionMatrix(predtest,iriste[,5])