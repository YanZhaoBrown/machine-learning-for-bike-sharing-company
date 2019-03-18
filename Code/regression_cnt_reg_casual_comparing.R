reg_alldayset = dayset[c(1:27,32:34,36:39,41)]
reg_alldayset.train = dayset.train[c(1:27,32:34,36:39,41)]
reg_alldayset.test = dayset.test[c(1:27,32:34,36:39,41)]

reg_alldayset_Tree = tree(cnt~.,cnt_alldayset.train)
summary(reg_alldayset_Tree)

plot(reg_alldayset_Tree)
text(reg_alldayset_Tree,pretty=0)

reg_alldayset_CVTree = cv.tree(reg_alldayset_Tree)
plot(reg_alldayset_CVTree$size,reg_alldayset_CVTree$dev,type='b')

reg_alldayset_CVTree

reg_alldayset_PruneTree = prune.tree(reg_alldayset_Tree, best=10)

plot(reg_alldayset_PruneTree)
text(reg_alldayset_PruneTree,pretty=0)

set.seed(2)
reg_alldayset_Forest = randomForest(registered~., data=reg_alldayset, newdata=reg_alldayset.train)
reg_alldayset_Forest
summary(reg_alldayset_Forest)
plot(reg_alldayset_Forest)

reg_alldayset.train.pred = reg_alldayset.train
reg_alldayset.train.pred$registered = NULL
reg_alldayset_CVForest = rfcv(reg_alldayset.train.pred,reg_alldayset.train$registered,cv.fold = 10)
with(reg_alldayset_CVForest, plot(n.var, error.cv, log="x", type="o", lwd=2))
title("ImPortant Variables",line=3)
reg_alldayset_CVForest$error.cv

reg_alldayset_CVForest_result = randomForest(registered~., data=reg_alldayset, newdata=reg_alldayset.train,mtry=17)
plot(reg_alldayset_CVForest_result)
plot(reg_alldayset_CVForest_result)
reg_alldayset_CVForest_result$importance
varImpPlot(reg_alldayset_CVForest_result,n.var = 17,color = "red",font = 2)
title("ImPortant Variables",line=3)

# How does each do on the testing data?
regp_alldayset_Tree = predict(reg_alldayset_Tree, newdata=reg_alldayset.test)
mean((regp_alldayset_Tree - reg_alldayset.test$registered)^2)

regp_alldayset_Prune_Tree = predict(reg_alldayset_PruneTree, newdata=reg_alldayset.test)
mean((regp_alldayset_Prune_Tree - reg_alldayset.test$registered)^2)

regp_alldayset_Forest = predict(reg_alldayset_Forest, newdata=reg_alldayset.test)
mean((regp_alldayset_Forest - reg_alldayset.test$registered)^2)

regp_alldayset_CVForest_result = predict(reg_alldayset_CVForest_result, newdata=reg_alldayset.test)
mean((regp_alldayset_CVForest_result - reg_alldayset.test$registered)^2)


######All lables with Casual#######
cas_alldayset = dayset[c(1:27,32:34,36:39,40)]
cas_alldayset.train = dayset.train[c(1:27,32:34,36:39,40)]
cas_alldayset.test = dayset.test[c(1:27,32:34,36:39,40)]

cas_alldayset_Tree = tree(casual~.,cas_alldayset.train)
summary(cas_alldayset_Tree)

plot(cas_alldayset_Tree)
text(cas_alldayset_Tree,pretty=0)

cas_alldayset_CVTree = cv.tree(cas_alldayset_Tree)
plot(cas_alldayset_CVTree$size,cas_alldayset_CVTree$dev,type='b')

cas_alldayset_CVTree

cas_alldayset_PruneTree = prune.tree(cas_alldayset_Tree, best=13)

plot(cas_alldayset_PruneTree)
text(cas_alldayset_PruneTree,pretty=0)

set.seed(2)
cas_alldayset_Forest = randomForest(casual~., data=cas_alldayset, newdata=cas_alldayset.train)
cas_alldayset_Forest
summary(cas_alldayset_Forest)
plot(cas_alldayset_Forest)

cas_alldayset.train.pred = cas_alldayset.train
cas_alldayset.train.pred$casual = NULL
cas_alldayset_CVForest = rfcv(cas_alldayset.train.pred,cas_alldayset.train$casual,cv.fold = 10)
with(cas_alldayset_CVForest, plot(n.var, error.cv, log="x", type="o", lwd=2))
cas_alldayset_CVForest$error.cv

cas_alldayset_CVForest_result = randomForest(casual~., data=cas_alldayset, newdata=cas_alldayset.train,mtry=17)
plot(cas_alldayset_CVForest_result)
plot(cas_alldayset_CVForest_result)
cas_alldayset_CVForest_result$importance
varImpPlot(cas_alldayset_CVForest_result,n.var = 17,color = "red",font = 2)
title("ImPortant Variables",line=3)

# How does each do on the testing data?
casp_alldayset_Tree = predict(cas_alldayset_Tree, newdata=cas_alldayset.test)
mean((casp_alldayset_Tree - cas_alldayset.test$casual)^2)

casp_alldayset_Prune_Tree = predict(cas_alldayset_PruneTree, newdata=cas_alldayset.test)
mean((casp_alldayset_Prune_Tree - cas_alldayset.test$casual)^2)

casp_alldayset_Forest = predict(cas_alldayset_Forest, newdata=cas_alldayset.test)
mean((casp_alldayset_Forest - cas_alldayset.test$casual)^2)

casp_alldayset_CVForest_result = predict(cas_alldayset_CVForest_result, newdata=cas_alldayset.test)
mean((casp_alldayset_CVForest_result - cas_alldayset.test$casual)^2)