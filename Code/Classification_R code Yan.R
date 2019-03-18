library(readr)
dayset = read.csv("day.csv")
attach(dayset)

###create binary variable cnt
dayset$cnt01[dayset$cnt >= mean(dayset$cnt)] = 1
dayset$cnt01[dayset$cnt < mean(dayset$cnt)] = 0
dayset = data.frame(dayset, dayset$cnt01)



###split cnt using 2-means method
set.seed(1)
cntCluster=kmeans(cnt,2,nstart=731)
cntCluster$cluster
cluster=cntCluster$cluster-1
cluster
plot(cnt, col=(2+cluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
points(cnt,col=(2+cnt01), pch=7)
dayset = data.frame(dayset,cluster)






###split registered and casual using 2-means method
set.seed(1)
regiCluster=kmeans(registered,2,nstart=731)
regicluster=regiCluster$cluster-1
plot(registered, col=(2+regicluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="registered", pch=6, cex=2)
dayset = data.frame(dayset,regicluster)

casCluster=kmeans(casual,2,nstart=731)
cascluster=casCluster$cluster - 1
plot(casual, col=(2+cascluster), main="K-Means Clustering Results with K=2", xlab="instant", ylab="casual", pch=6, cex=2)
dayset = data.frame(dayset,cascluster)



#normalize cnt
dayset$cnt1=(dayset$cnt-min(dayset$cnt))/(max(dayset$cnt)-min(dayset$cnt))
dayset=data.frame(dayset,dayset$cnt1)


# Split season
season_rows = data.frame(spring=logical(0), summer=logical(0), fall=logical(0), winter=logical(0))

for (r in 1:nrow(dayset))
{
  season_number = dayset$season[r]
  
  if (season_number == 1) # spring
    season_row = data.frame(spring=1,summer=0,fall=0,winter=0)
  else if (season_number == 2) # summer
    season_row = data.frame(spring=0,summer=1,fall=0,winter=0)
  else if (season_number == 3) # fall
    season_row = data.frame(spring=0,summer=0,fall=1,winter=0)
  else if (season_number == 4) # winter
    season_row = data.frame(spring=0,summer=0,fall=0,winter=1)
  
  season_rows = rbind(season_row,season_rows)
  
}

dayset = cbind(season_rows, dayset)



#new idea for season
season_rows = data.frame(s1=logical(0), s2=logical(0))

for (r in 1:nrow(dayset))
{
  season_number = dayset$season[r]
  
  if (season_number == 1) # spring
    season_row = data.frame(s1=1,s2=0)
  else if (season_number == 2) # summer
    season_row = data.frame(s2=0,s1=1)
  else if (season_number == 3) # fall
    season_row = data.frame(s1=-1,s2=0)
  else if (season_number == 4) # winter
    season_row = data.frame(s1=0,s2=-1)
  
  season_rows = rbind(season_row,season_rows)
  
}

dayset = cbind(season_rows, dayset)






#split weathersit
weathersit_rows = data.frame(clear=integer(0), mist=integer(0), light_snow_rain=integer(0), heavy_rain=integer(0))

for (r in 1:nrow(dayset))
{
  weathersit_number = dayset$weathersit[r]
  
  #weathersit : 
  #  1: Clear + Few clouds + Partly cloudy
  # 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
  # 3: Light Snow + Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
  # 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
  
  
  if (weathersit_number == 1) # Clear
    weathersit_row = data.frame(clear=1,mist=0,light_snow_rain=0,heavy_rain=0)
  else if (weathersit_number == 2) # Mist
    weathersit_row = data.frame(clear=0,mist=1,light_snow_rain=0,heavy_rain=0)
  else if (weathersit_number == 3) # Light Snow
    weathersit_row = data.frame(clear=0,mist=0,light_snow_rain=1,heavy_rain=0)
  else if (weathersit_number == 4) # Heavy Rain
    weathersit_row = data.frame(clear=0,mist=0,light_snow_rain=0,heavy_rain=1)
  else
    print("Invalid Weather Situation")
  
  weathersit_rows = rbind(weathersit_rows, weathersit_row)
}

dayset = cbind(weathersit_rows, dayset)




# Split Month
month_rows = data.frame(january=logical(0), february=logical(0), march=logical(0),
                        april=logical(0), may=logical(0), june=logical(0),
                        july=logical(0), august=logical(0), september=logical(0),
                        october=logical(0), november=logical(0), december=logical(0))

for (r in 1:nrow(dayset))
{
  month_number = dayset$mnth[r]
  
  if (month_number == 1) # January
    month_row = data.frame(january=1, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 2) # February
    month_row = data.frame(january=0, february=1, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 3) # March
    month_row = data.frame(january=0, february=0, march=1, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 4) # April
    month_row = data.frame(january=0, february=0, march=0, april=1, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 5) # May
    month_row = data.frame(january=0, february=0, march=0, april=0, may=1, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 6) # June
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=1,
                           july=0, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 7) # July
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=1, august=0, september=0, october=0, november=0, december=0)
  else if (month_number == 8) # August
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=1, september=0, october=0, november=0, december=0)
  else if (month_number == 9) # September
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=1, october=0, november=0, december=0)
  else if (month_number == 10) # October
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=1, november=0, december=0)
  else if (month_number == 11) # November
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=1, december=0)
  else if (month_number == 12) # December
    month_row = data.frame(january=0, february=0, march=0, april=0, may=0, june=0,
                           july=0, august=0, september=0, october=0, november=0, december=1)
  else
    print("Invalid Month")
  
  month_rows = rbind(month_rows, month_row)
}

dayset = cbind(month_rows, dayset)



# Split Weekday

weekday_rows = data.frame(sunday=logical(0), monday=logical(0), tuesday=logical(0),
                          wednesday=logical(0), thursday=logical(0), friday=logical(0),
                          saturday=logical(0))

for (r in 1:nrow(dayset))
{
  weekday_number = dayset$weekday[r]
  
  if (weekday_number == 0) # Sunday
    weekday_row = data.frame(sunday=1,monday=0,tuesday=0,wednesday=0,
                             thursday=0,friday=0,saturday=0)
  else if (weekday_number == 1) # Monday
    weekday_row = data.frame(sunday=0,monday=1,tuesday=0,wednesday=0,
                             thursday=0,friday=0,saturday=0)
  else if (weekday_number == 2) # Tuesday
    weekday_row = data.frame(sunday=0,monday=0,tuesday=1,wednesday=0,
                             thursday=0,friday=0,saturday=0)
  else if (weekday_number == 3) # Wednesday
    weekday_row = data.frame(sunday=0,monday=0,tuesday=0,wednesday=1,
                             thursday=0,friday=0,saturday=0)
  else if (weekday_number == 4) # Thursday
    weekday_row = data.frame(sunday=0,monday=0,tuesday=0,wednesday=0,
                             thursday=1,friday=0,saturday=0)
  else if (weekday_number == 5) # Friday
    weekday_row = data.frame(sunday=0,monday=0,tuesday=0,wednesday=0,
                             thursday=0,friday=1,saturday=0)
  else if (weekday_number == 6) # Saturday
    weekday_row = data.frame(sunday=0,monday=0,tuesday=0,wednesday=0,
                             thursday=0,friday=0,saturday=1)
  else
    print("Invalid Weekday")
  
  weekday_rows = rbind(weekday_rows, weekday_row)
}

dayset = cbind(weekday_rows, dayset)




#seperate the data set as training data and test data
set.seed(1)
train.size=dim(dayset)[1]*3/4
samples = sample(1:nrow(dayset), train.size, replace=FALSE) 
train = logical(nrow(dayset))
train[samples] = TRUE
test = !train
day_train = dayset[train,]
day_test = dayset[test,]



par(mfrow=c(1,1))
#plot
#cnt change with time
plot(day_train$instant,day_train$cnt1,col=1+day_train$cluster, pch=8)
#cnt change with atemp
plot(day_train$atemp,day_train$hum,col=1+day_train$cluster,pch=8)
plot(day_train$atemp,day_train$windspeed,col=1+day_train$cluster,pch=8)
#cnt with change of season
plot(day_train$cnt1,day_train$winter, col=2+day_train$winter)
plot(day_train$cnt1,day_train$summer, col=2+day_train$summer)
#cnt with change of year
plot(day_train$cnt1,day_train$temp,col=1+day_train$yr,pch=8)
plot(day_train$cnt1,day_train$yr, col=2+day_train$yr)
#cnt change with weekday, no obvious trend
plot(day_train$cnt1,day_train$temp,col=2+day_train$weekday,pch=8)
#cnt change holiday, no obvious trend
plot(day_train$cnt1,day_train$temp,col=1+day_train$holiday,pch=8)

#registered & casual
plot(day_train$instant,day_train$casual,col=1+day_train$registered01,pch=8)
plot(day_train$instant,day_train$registered,col=1+day_train$casual01,pch=8)
plot(day_train$instant,day_train$cnt,col=1+day_train$registered01,pch=8)
plot(day_train$instant,day_train$cnt,col=1+day_train$casual01,pch=8)



#LDA & QDA & knn & logistic
library(class)
library(MASS)



###classfication of cnt01 on the whole predictors

#LDA
set.seed(1)
lda.fit=lda(cnt01~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$cnt01)
(11+16)/183
0.147

#QDA
set.seed(1)
qda.fit=qda(cnt01~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(lda.pred$class,day_test$cnt01)
(11+16)/183
0.147

#logistic
set.seed(1)
glm.fit=glm(cnt01~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cnt01)
(11+16)/183
0.147

#knn--worst one for the whole data
set.seed(1)
train.x=cbind(yr, season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train,]
test.x=cbind(yr, season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test,]

errors=NULL
for (i in 1:100) {
  knn.pred=knn(train.x, test.x, day_train$cnt01,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$cnt01)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.169



### predict of clustering cnt using all of the predictors
#LDA
set.seed(1)
lda.fit=lda(cluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$cluster)
(10+15)/183
0.137



#QDA
set.seed(1)
qda.fit=qda(cluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(lda.pred$class,day_test$cluster)
(10+15)/183
0.137




#logistic
set.seed(1)
glm.fit=glm(cluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(10+15)/183
0.137



#knn
set.seed(1)
train.x=cbind(yr, season, mnth, weekday, weathersit
              , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train,]
test.x=cbind(yr, season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test,]

errors=NULL
for (i in 1:100) {
  knn.pred=knn(train.x, test.x, day_train$cluster,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$cluster)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.169




#classification using new classification of season
set.seed(1)
lda.fit=lda(cluster~ yr + s1 + s2 + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$cluster)
(11+16)/183
0.148



set.seed(1)
qda.fit=qda(cluster~ yr + s1 + s2 + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(lda.pred$class,day_test$cluster)
(11+16)/183
0.148



set.seed(1)
glm.fit=glm(cluster~ yr + s1 + s2 + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(11+17)/183
0.153



set.seed(1)
train.x=cbind(yr, s1, s2, mnth, weekday, weathersit
              , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train,]
test.x=cbind(yr, s1, s2, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test,]

errors=NULL
for (i in 1:100) {
  knn.pred=knn(train.x, test.x, day_train$cluster,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$cluster)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.158





#registerd
attach(dayset)
set.seed(1)
lda.fit=lda(regicluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$regicluster)
(13+15)/183
0.153


set.seed(1)
qda.fit=qda(regicluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(qda.pred$class,day_test$regicluster)
(15+14)/183
0.158



set.seed(1)
glm.fit=glm(regicluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$regicluster)
(14+16)/183
0.164


set.seed(1)
train.x=cbind(yr, season, mnth, weekday, weathersit
              , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train,]
test.x=cbind(yr, season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test,]

errors=NULL
for (i in 1:100) {
  knn.pred=knn(train.x, test.x, day_train$regicluster,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$regicluster)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.186



#casual
set.seed(1)
lda.fit=lda(cascluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$cascluster)
(7+9)/183
0.087



set.seed(1)
qda.fit=qda(cascluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(qda.pred$class,day_test$cascluster)
(11+11)/183
0.12



set.seed(1)
glm.fit=glm(cascluster~ yr + season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cascluster)
(10+8)/183
0.098


set.seed(1)
train.x=cbind(yr, season, mnth, weekday, weathersit
              , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train,]
test.x=cbind(yr, season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test,]

errors=NULL
for (i in 1:100) {
  knn.pred=knn(train.x, test.x, day_train$cascluster,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$cascluster)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.0765






#We do whole data set based on the 2nd yr -- in order to minimize the error

#construct new dayset2
dayset2=subset(dayset,dayset$yr==1)

###split cnt using 2-means method
attach(dayset2)
set.seed(2)
cntCluster2=kmeans(cnt,2,nstart=366)
cntCluster2$cluster
cluster2=cntCluster2$cluster-1
cluster2
plot(cnt, col=(2+cluster2), main="K-Means Clustering Results with K=2", xlab="instant", ylab="cnt", pch=6, cex=2)
points(cnt,col=(2+cnt01), pch=7)
dayset2 = data.frame(dayset2,cluster2)


#split as training and test
set.seed(1)
train.size2=dim(dayset2)[1]*3/4
samples = sample(1:nrow(dayset2), train.size2, replace=FALSE) 
train2 = logical(nrow(dayset2))
train2[samples] = TRUE
test2 = !train2
day_train2 = dayset2[train2,]
day_test2 = dayset2[test2,]


#LDA
attach(dayset2)
set.seed(1)
lda.fit2=lda(cluster2~ season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset2, subset=train)
lda.pred2=predict(lda.fit2,day_test2)
table(lda.pred2$class,day_test2$cluster2)
(2+8)/92
0.109



#QDA
set.seed(1)
qda.fit2=qda(cluster2~ season + mnth + weekday + weathersit
             + holiday + weekday + workingday + temp + atemp + hum + windspeed
             ,data=dayset2, subset=train)
qda.pred2=predict(qda.fit2,day_test2)
table(qda.pred2$class,day_test2$cluster2)
(3+6)/92
0.098



#logistic
set.seed(1)
glm.fit2=glm(cluster2~ season + mnth + weekday + weathersit
            + holiday + weekday + workingday + temp + atemp + hum + windspeed
            ,data=dayset2, family=binomial,subset=train)
glm.probs=predict(glm.fit2,day_test2,type="response")
glm.pred=rep("0",92)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test2$cluster2)
(2+6)/92
0.087


#knn
set.seed(1)
train.x2=cbind(season, mnth, weekday, weathersit
              , holiday, weekday, workingday, temp, atemp, hum, windspeed)[train2,]
test.x2=cbind(season, mnth, weekday, weathersit
             , holiday, weekday, workingday, temp, atemp, hum, windspeed)[test2,]

errors=NULL
for (i in 1:50) {
  knn.pred=knn(train.x2, test.x2, day_train2$cluster2,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test2$cluster2)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.098





### winter
set.seed(1)
glm.fit=glm(cluster~winter,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(32+85)/183
0.64

### summer
set.seed(1)
glm.fit=glm(cluster~summer,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(75+36)/183
0.61




### atemp
set.seed(1)
glm.fit=glm(cluster~atemp,data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(64+72)/183
0.743






### PCA
{
  dayset.pca_in = dayset[c('instant','season',
                           'yr','mnth','holiday','weekday','workingday','weathersit','temp','atemp','hum','windspeed','casual','registered','cnt')]
  
  dayset.scaled = scale(dayset.pca_in, center=TRUE, scale=TRUE)  # normalize dayset data
  dayset.svd = svd(dayset.scaled)
  dayset.pca_T = dayset.svd$u %*% diag(dayset.svd$d)    # T matrix is transformed data points
  dayset=data.frame(dayset, dayset.pca_T)
  
  ##split test and training
  set.seed(1)
  day_train = dayset[train,]
  day_test = dayset[test,]
  
  
  
  # Plot PCA results
  sdev = NULL
  cumulative_variance = NULL
  sum = 0
  
  for (p in 1:length(dayset.pca_T[1,]))
  {
    sdev[p] = sd(dayset.pca_T[,p])
    sum = sum + sdev[p]^2
    cumulative_variance[p] = sum
  }
  
  par(mfrow=c(1,2))
  plot(sdev^2, xlab='PCA Predictor', ylab='Variance')
  plot(cumulative_variance, xlab='Number of PCA Predictors', ylab='Cumulative Variance')  
}  



#classification
attach(dayset)
set.seed(1)
lda.fit=lda(cluster~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 , data= dayset, subset=train)
lda.pred=predict(lda.fit,day_test)
table(lda.pred$class,day_test$cluster)
(1+5)/183
0.033


qda.fit=qda(cluster~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 , data= dayset, subset=train)
qda.pred=predict(qda.fit,day_test)
table(qda.pred$class,day_test$cluster)
(3+16)/183
0.104


glm.fit=glm(cluster~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12, data=dayset, family=binomial,subset=train)
glm.probs=predict(glm.fit,day_test,type="response")
glm.pred=rep("0",183)
glm.pred[glm.probs>.5]="1"
table(glm.pred, day_test$cluster)
(1+2)/183
0.016


train.x=cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12)[train,]
test.x=cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12)[test2,]

errors=NULL
for (i in 1:11) {
  knn.pred=knn(train.x, test.x, day_train$cluster,k=i)
  
  errors[i] = 1-mean(knn.pred==day_test$cluster)
}
k=which.min(errors)
k
errors=min(errors)
errors
0.17