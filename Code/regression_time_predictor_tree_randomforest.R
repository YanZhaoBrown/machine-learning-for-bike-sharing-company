library(tree)
library(randomForest)
library(ggplot2)

#names(dayset)
#[1] "sunday"          "monday"          "tuesday"         "wednesday"       "thursday"        "friday"         
#[7] "saturday"        "january"         "february"        "march"           "april"           "may"            
#[13] "june"            "july"            "august"          "september"       "october"         "november"       
#[19] "december"        "clear"           "mist"            "light_snow_rain" "heavy_rain"      "spring"         
#[25] "summer"          "fall"            "winter"          "instant"         "season"          "yr"             
#[31] "mnth"            "holiday"         "weekday"         "workingday"      "weathersit"      "temp"           
#[37] "atemp"           "hum"             "windspeed"       "casual"          "registered"      "cnt" 

######2 years######
######All lables with cnt##########

cnt_alldayset = dayset[c(1:19,24:27,32:34,42)]
cnt_alldayset.train = dayset.train[c(1:19,24:27,32:34,42)]
cnt_alldayset.test = dayset.test[c(1:19,24:27,32:34,42)]

#########################################################################

cnt_alldayset_Tree = tree(cnt~.,cnt_alldayset.train)
summary(cnt_alldayset_Tree)

plot(cnt_alldayset_Tree)
text(cnt_alldayset_Tree,pretty=0,col='red')
title("Tree with Time Predictors")

set.seed(2)
cnt_alldayset_CVTree = cv.tree(cnt_alldayset_Tree)
plot(cnt_alldayset_CVTree$size,cnt_alldayset_CVTree$dev,type='b',ylab = "Deviation",xlab = "Tree Size")
title("Cross Validation for Tree")

cnt_alldayset_CVTree

cnt_alldayset_PruneTree = prune.tree(cnt_alldayset_Tree, best=8)
cnt_alldayset_PruneTree
plot(cnt_alldayset_PruneTree)
text(cnt_alldayset_PruneTree,pretty=0,col = 'red',font = 2)
title("Best PruneTree with Time Predictors")

cntp_alldayset_Prune_Tree = predict(cnt_alldayset_PruneTree, newdata=cnt_alldayset.test)
cntp_alldayset_Prune_Tree.train = predict(cnt_alldayset_PruneTree, newdata=cnt_alldayset.train)
test.error=mean((cntp_alldayset_Prune_Tree - cnt_alldayset.test$cnt)^2)
train.error=mean((cntp_alldayset_Prune_Tree.train - cnt_alldayset.train$cnt)^2)
test.error
train.error

plot(dayset$instant, cnt_alldayset$cnt, xlab="Time (Days)", ylab="Bikeshare Total Users")
points(dayset.train$instant, cntp_alldayset_Prune_Tree.train, col="blue")
points(dayset.test$instant, cntp_alldayset_Prune_Tree, col="red", lwd=3)

mtext(paste("test.error:", test.error, sep = ""),side = 3,line = 1,adj = 1)
mtext(paste("tree.size:",'8' , sep = ""),side = 3,line = 2,adj = 1)
mtext('red: testing prediction', line=2,adj=0,col="red")
mtext('blue: Training prediction', line=1,adj=0,col="Blue")






### 
{
  prunetree_results = data.frame(treesize=integer(0), train.error=numeric(0), test.error=numeric(0))
  
  
  for (treesize in 2:8)
  {
      cnt_alldayset_PruneTree = prune.tree(cnt_alldayset_Tree, best=treesize)
      
      cntp_alldayset_Prune_Tree = predict(cnt_alldayset_PruneTree, newdata=cnt_alldayset.test)
      cntp_alldayset_Prune_Tree.train = predict(cnt_alldayset_PruneTree, newdata=cnt_alldayset.train)
      train.error=mean((cntp_alldayset_Prune_Tree.train - cnt_alldayset.train$cnt)^2)
      test.error=mean((cntp_alldayset_Prune_Tree - cnt_alldayset.test$cnt)^2)
      
      #png(paste("PruneTree_", treesize, ".png", sep=""))
      #plot(dayset$instant, cnt_alldayset$cnt, xlab="Time (Days)", ylab="Bikeshare Total Users")
     # points(dayset.train$instant, cntp_alldayset_Prune_Tree.train, col="blue")
      #points(dayset.test$instant, cntp_alldayset_Prune_Tree, col="red", lwd=3)
      #mtext(paste("test.error:", test.error, sep = ""),side = 3,line = 1,adj = 1, col="red")
      #mtext(paste("tree.size:",treesize , sep = ""),side = 3,line = 2,adj = 1, col="blue")
      #dev.off()
      #################
      
      # save test and train error results for each trial
      prunetree_results = rbind(prunetree_results, cbind(treesize, train.error, test.error))
  }
}

plot(x=prunetree_results$treesize,y=prunetree_results$train.error,pch = 8,col='blue')
lines(x=prunetree_results$treesize,y=prunetree_results$train.error,col='blue')
points(x=prunetree_results$treesize,y=prunetree_results$test.error,pch = 8,col='red')
lines(x=prunetree_results$treesize,y=prunetree_results$test.error,pch = 8,col='red')
mtext("Red: Testing error",adj = 1, line = 2,col = 'red')
mtext("Blue:Training error",adj = 1, line = 1,col = 'Blue')


















####################Random Forest#########################################

set.seed(2)
cnt_alldayset_Forest = randomForest(cnt~., data=cnt_alldayset.train)
cnt_alldayset_Forest
summary(cnt_alldayset_Forest)
plot(cnt_alldayset_Forest)

cnt_alldayset.train.pred = cnt_alldayset.train
cnt_alldayset.train.pred$cnt = NULL
set.seed(2)
cnt_alldayset_CVForest = rfcv(cnt_alldayset.train.pred,cnt_alldayset.train$cnt,cv.fold = 10)
with(cnt_alldayset_CVForest, plot(n.var, error.cv, log="x", type="o", lwd=2,
                                  ylab = "Cross Validation Error",xlab = "Number of Variables",font = 2))
title("Cross Validation for Random Forest")
cnt_alldayset_CVForest$error.cv

cnt_alldayset_CVForest_result = randomForest(cnt~., data=cnt_alldayset, newdata=cnt_alldayset.train,mtry=13)
plot(cnt_alldayset_CVForest_result)
cnt_alldayset_CVForest_result$importance
varImpPlot(cnt_alldayset_CVForest_result,n.var = 13,color = "red",font = 2)
title("ImPortant Variables",line=3)




cntp_alldayset_Forest = predict(cnt_alldayset_Forest, newdata=cnt_alldayset.test)
mean((cntp_alldayset_Forest - cnt_alldayset.test$cnt)^2)

cntp_alldayset_CVForest_result = predict(cnt_alldayset_CVForest_result, newdata=cnt_alldayset.test)
cntp_alldayset_CVForest_result.train = predict(cnt_alldayset_CVForest_result, newdata=cnt_alldayset.train)
train.error = mean((cntp_alldayset_CVForest_result.train - cnt_alldayset.train$cnt)^2)
test.error=mean((cntp_alldayset_CVForest_result - cnt_alldayset.test$cnt)^2)
train.error
test.error

png(paste("Randomforset_cnt_", "trial", ".png", sep=""))
plot(dayset$instant, cnt_alldayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
points(dayset.train$instant, cntp_alldayset_CVForest_result.train, col="blue")
points(dayset.test$instant, cntp_alldayset_CVForest_result, col="red", pch=18,lwd=6)
mean((cntp_alldayset_CVForest_result - cnt_alldayset.test$cnt)^2)
dev.off()


{
  cvrandomforest_results = data.frame(variable.num=integer(0), train.error=numeric(0), test.error=numeric(0))
  
  
  for (variable.num in 1:20)
  {
    cnt_alldayset_CVForest_result = randomForest(cnt~., data=cnt_alldayset, newdata=cnt_alldayset.train,mtry=variable.num)
    
    cntp_alldayset_CVForest_result = predict(cnt_alldayset_CVForest_result, newdata=cnt_alldayset.test)
    cntp_alldayset_CVForest_result.train = predict(cnt_alldayset_CVForest_result, newdata=cnt_alldayset.train)
    train.error = mean((cntp_alldayset_CVForest_result.train - cnt_alldayset.train$cnt)^2)
    test.error=mean((cntp_alldayset_CVForest_result - cnt_alldayset.test$cnt)^2)
    
    png(paste("Randomforset_variable#_",variable.num, ".png", sep=""))
    plot(dayset$instant, cnt_alldayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
    points(dayset.train$instant, cntp_alldayset_CVForest_result.train, col="blue")
    points(dayset.test$instant, cntp_alldayset_CVForest_result, col="red", pch=18,lwd=6)
    mtext(paste("test.error:", test.error, sep = ""),side = 3,line = 1,adj = 1, col="red")
    mtext(paste("Num of Variables:",variable.num , sep = ""),side = 3,line = 2,adj = 1, col="blue")
    dev.off()
    
    #################
    
    # save test and train error results for each trial
    cvrandomforest_results = rbind(cvrandomforest_results, cbind(variable.num, train.error, test.error))
  }
}
plot(x=cvrandomforest_results$variable.num,y=cvrandomforest_results$test.error,pch = 8,col='red')
points(x=cvrandomforest_results$variable.num,y=cvrandomforest_results$train.error,pch = 8,col='blue')
lines(x=cvrandomforest_results$variable.num,y=cvrandomforest_results$train.error,col='blue')
lines(x=cvrandomforest_results$variable.num,y=cvrandomforest_results$test.error,pch = 8,col='red')

varImpPlot(cnt_alldayset_CVForest_result,n.var = 10,color = "red",font = 2)
