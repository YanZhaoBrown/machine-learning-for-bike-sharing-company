rm(list=ls())
library(foreign)
library(tree)
library(rpart)
library(grid)
library(partykit)
library(party)

getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

set.seed(1)
cntCluster=kmeans(cnt,2,nstart=731)
#cntCluster
Cluster=cntCluster$cluster-1
#Cluster
cluster = ifelse(Cluster == 0, "Low", "High")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#dayset

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)

#Seperate data to training data and test data.
set.seed(1)
train.size1=dim(dayset)[1] * 3/4
samples = sample(1:nrow(dayset), train.size1, replace=FALSE)
train = logical(nrow(dayset))
train[samples] = TRUE
test = !train
dayset.train = dayset[train,]
dayset.test = dayset[test,]

dim(dayset.train)
dim(dayset.test)
#summary(dayset.train)
#summary(dayset.test)

#------------------------------------------------------------------------------------------#
#Classificationn Tree,y = cnt
set.seed(1)
TModel <-  rpart(cluster ~., method="class", data=dayset.train)
TModel

#attach(dayset.test)
tree.pred.cnt <- predict(TModel,dayset.test, type = "class")
MTable = as.matrix(table(tree.pred.cnt, dayset.test$cluster))
SMT = sum(MTable)
diag = diag(MTable)
accuracy = sum(diag) / SMT 
error_rate= 1-accuracy
MTable 
accuracy
error_rate
rpart.plot(TModel)

printcp(TModel)
plotcp(TModel)
print(TModel)


#prune tree
PTree=prune(TModel,cp= TModel$cptable[which.min(TModel$cptable[,"xerror"]),"CP"])
PTree
prune.pred.cnt <- predict(PTree,dayset.test, type = "class")
P_MTable = as.matrix(table(prune.pred.cnt, dayset.test$cluster))
P_SMT = sum(P_MTable)
P_diag = diag(P_MTable)
P_accuracy = sum(P_diag) / P_SMT 
P_error_rate= 1-P_accuracy
P_MTable
P_accuracy
P_error_rate

rpart.plot(PTree)

############################################################################################
#Classificationn Tree,y = registered
rm(list=ls())
getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

set.seed(1)
registeredCluster=kmeans(registered,2,nstart=731)
#registeredCluster
Cluster=registeredCluster$cluster-1
#Cluster
cluster = ifelse(Cluster == 0, "Low", "High")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#View(dayset)

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)

#Seperate data to training data and test data.
set.seed(1)
train.size1=dim(dayset)[1] * 3/4
samples = sample(1:nrow(dayset), train.size1, replace=FALSE)
train = logical(nrow(dayset))
train[samples] = TRUE
test = !train
dayset.train = dayset[train,]
dayset.test = dayset[test,]

dim(dayset.train)
dim(dayset.test)
#summary(dayset.train)
#summary(dayset.test)

#------------------------------------------------------------------------------------------#
set.seed(1)
TModel1 <-  rpart(cluster ~., method="class", data=dayset.train)
TModel1
rpart.plot(TModel1)

tree.pred.registered <- predict(TModel1,dayset.test, type = "class")
MTable = as.matrix(table(tree.pred.registered, dayset.test$cluster))
SMT = sum(MTable)
diag = diag(MTable)
accuracy = sum(diag) / SMT 
error_rate= 1-accuracy
MTable 
accuracy
error_rate

printcp(TModel1)
plotcp(TModel1)
print(TModel1)


#prune tree
PTree1=prune(TModel1,cp= TModel1$cptable[which.min(TModel1$cptable[,"xerror"]),"CP"])
PTree1
prune.pred.registered <- predict(PTree1,dayset.test, type = "class")
P_MTable = as.matrix(table(prune.pred.registered, dayset.test$cluster))
P_SMT = sum(P_MTable)
P_diag = diag(P_MTable)
P_accuracy = sum(P_diag) / P_SMT 
P_error_rate= 1-P_accuracy
P_MTable
P_accuracy
P_error_rate

rpart.plot(PTree1)

#######################################################################################
#Classificationn Tree,y = casual
rm(list=ls())
getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

set.seed(1)
casualCluster=kmeans(casual,2,nstart=731)
#casualCluster
#casualCluster$cluster
Cluster=casualCluster$cluster-1
#Cluster
cluster = ifelse(Cluster == 0, "Low", "High")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#View(dayset)

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)

#Seperate data to training data and test data.
set.seed(1)
train.size1=dim(dayset)[1] * 3/4
samples = sample(1:nrow(dayset), train.size1, replace=FALSE)
train = logical(nrow(dayset))
train[samples] = TRUE
test = !train
dayset.train = dayset[train,]
dayset.test = dayset[test,]

dim(dayset.train)
dim(dayset.test)
summary(dayset.train)
#summary(dayset.test)

#------------------------------------------------------------------------------------------#
set.seed(1)
TModel2 <-  rpart(cluster~., method="class", data=dayset.train)
TModel2
rpart.plot(TModel2)

tree.pred.casual <- predict(TModel2,dayset.test, type = "class")
MTable = as.matrix(table(tree.pred.casual, dayset.test$cluster))
SMT = sum(MTable)
diag = diag(MTable)
accuracy = sum(diag) / SMT 
error_rate= 1-accuracy
MTable 
accuracy
error_rate

printcp(TModel2)
plotcp(TModel2)
print(TModel2)

#prune tree
PTree2=prune(TModel2,cp= TModel2$cptable[which.min(TModel2$cptable[,"xerror"]),"CP"])
PTree2
prune.pred.casual<- predict(PTree2,dayset.test, type = "class")
P_MTable = as.matrix(table(prune.pred.casual, dayset.test$cluster))
P_SMT = sum(P_MTable)
P_diag = diag(P_MTable)
P_accuracy = sum(P_diag) / P_SMT 
P_error_rate= 1-P_accuracy
P_MTable
P_accuracy
P_error_rate

rpart.plot(PTree2)

############################################################################################
#Random Forest-cnt
rm(list=ls())

library(MASS)
library(randomForest)
library(tree)
#attach(dayset.train)
#attach(dayset.test)

getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

attach(dayset)
set.seed(1)
cntCluster=kmeans(cnt,2,nstart=731)
#cntCluster
#cntCluster$cluster
Cluster=cntCluster$cluster-1
#Cluster
cluster = ifelse(Cluster == 0, "Low", "High")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#View(dayset)

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)


#------------------------------------------------------------------------------------------#
# Set the random seed
set.seed(1)

cnt_Forest = randomForest(cluster ~., data=dayset, mtry=sqrt(11),ntree=5000)
#summary(myForest)
cnt_Forest

# Some plots for discussion
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(cnt_Forest, log="x")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(cnt_Forest$err.rate),col=1:4,cex=0.8,fill=1:4)

cnt_Forest$importance
varImpPlot(cnt_Forest)

############################################################################################
#Random Forest-registered
rm(list=ls())

library(MASS)
library(randomForest)
library(tree)
#attach(dayset.train)
#attach(dayset.test)

getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

attach(dayset)
set.seed(1)
registeredCluster=kmeans(registered,2,nstart=731)
registeredCluster
registeredCluster$cluster
Cluster=registeredCluster$cluster-1
Cluster
cluster = ifelse(Cluster == 0, "Low", "High")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#View(dayset)

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)

#------------------------------------------------------------------------------------------#
# Set the random seed
set.seed(1)

registered_Forest = randomForest(cluster ~.,  data=dayset, mtry=sqrt(11),ntree=5000)
#summary(myForest)
registered_Forest

# Some plots for discussion
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(registered_Forest, log="x")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(registered_Forest$err.rate),col=1:4,cex=0.8,fill=1:4)

registered_Forest$importance
varImpPlot(registered_Forest)

############################################################################################
#Random Forest-casual
rm(list=ls())

library(MASS)
library(randomForest)
library(tree)
#attach(dayset.train)
#attach(dayset.test)

getwd()
setwd('/Users/Rebecca/Documents/MyWPI/2017 Spring/DS 502/Assignment/Final Project/DataSet')

dayset <- read.csv('day.csv')
#View(dayset)

set.seed(1)
casualCluster=kmeans(casual,2,nstart=731)
Cluster=casualCluster$cluster-1
cluster = ifelse(Cluster == 0, "Less", "More")
#plot(cnt, col=(1+cntCluster$cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
dayset = data.frame(dayset,cluster)
#View(dayset)

dayset$dteday = NULL
dayset$instant = NULL
dayset$registered = NULL
dayset$casual = NULL
dayset$cnt = NULL
names(dayset)



#------------------------------------------------------------------------------------------#
# Set the random seed
set.seed(1)

casual_Forest = randomForest(cluster ~.,data=dayset, mtry=sqrt(11),ntree=5000)
#summary(myForest)
casual_Forest

# Some plots for discussion
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(casual_Forest, log="x")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(casual_Forest$err.rate),col=1:4,cex=0.8,fill=1:4)


casual_Forest$importance
varImpPlot(casual_Forest)


