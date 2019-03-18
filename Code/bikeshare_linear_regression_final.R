# Bikeshare
# Linear Regression


library(foreign)
library(leaps)
library(MASS)
library(ISLR)
library(class)
library(glmnet)
library(boot)



# Read in Bikeshare data from the hour dataset and the day dataset

# set location where data is stored
setwd("~/College/Senior/C Term/Statistical Methods for Data Science/Final Project/Bike-Sharing-Dataset")

dayset = read.csv("day.csv")
hourset = read.csv("hour.csv")



### Encode Categorical Variables in Multiple Columns
# Predictors to split: season, mnth, weekday, weathersit

{
  # Split Season
  season_rows = data.frame(spring=logical(0), summer=logical(0), fall=logical(0), winter=logical(0))
  season_rows = cbind(season_rows,data.frame(spring_summer=integer(0), summer_fall=integer(0), fall_winter=integer(0), winter_spring=integer(0)))
  
  for (r in 1:nrow(dayset))
  {
    season_number = dayset$season[r]
    
    # Independent Season Vectors
    if (season_number == 1) # spring
      season_row = data.frame(spring=1,summer=0,fall=0,winter=0)
    else if (season_number == 2) # summer
      season_row = data.frame(spring=0,summer=1,fall=0,winter=0)
    else if (season_number == 3) # fall
      season_row = data.frame(spring=0,summer=0,fall=1,winter=0)
    else if (season_number == 4) # winter
      season_row = data.frame(spring=0,summer=0,fall=0,winter=1)
    else
      print("Invalid Season")
    
    # Consecutive Season Vectors
    if (season_number == 1) # spring
      season_row2 = data.frame(spring_summer=1,summer_fall=0,fall_winter=0,winter_spring=1)
    else if (season_number == 2) # summer
      season_row2 = data.frame(spring_summer=1,summer_fall=1,fall_winter=0,winter_spring=0)
    else if (season_number == 3) # fall
      season_row2 = data.frame(spring_summer=0,summer_fall=1,fall_winter=1,winter_spring=0)
    else if (season_number == 4) # winter
      season_row2 = data.frame(spring_summer=0,summer_fall=0,fall_winter=1,winter_spring=1)
    else
      print("Invalid Season")
    
    
    
    season_rows = rbind(season_rows, cbind(season_row, season_row2))
  }
  
  dayset = cbind(season_rows, dayset)
  
  
  
  # Split Weather Situation
  weathersit_rows = data.frame(clear=integer(0), mist=integer(0), light_snow_rain=integer(0), heavy_rain=integer(0))
  
  for (r in 1:nrow(dayset))
  {
    weathersit_number = dayset$weathersit[r]
    
    #weathersit : -----
    # 1: Clear + Few clouds + Partly cloudy
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
  
}

### Split data into Test and Training sets
{
  set.seed(1)
  
  train.size1=dim(hourset)[1] * 3/4
  samples = sample(1:nrow(hourset), train.size1, replace=FALSE)
  train = logical(nrow(hourset))
  train[samples] = TRUE
  test = !train
  hourset.train = hourset[train,]
  hourset.test = hourset[test,]
  
  train.size2=dim(dayset)[1] * 3/4
  samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
  train = logical(nrow(dayset))
  train[samples] = TRUE
  test = !train
  dayset.train = dayset[train,]
  dayset.test = dayset[test,]
  
  
  #dim(hourset.train)
  #dim(hourset.test)
  #summary(hourset.train)
  #summary(hourset.test)
}


### Linear Regression
### Random Train and Test Sets
{
  trials_results = data.frame(trial=integer(0), seed=integer(0), train_error=numeric(0), test_error=numeric(0))
  
  
  for (trial in 1:20)
  {
    seed = trial
    
    # Split data into train and test sets
    {
      set.seed(seed)
      
      train.size2=dim(dayset)[1] * 3/4
      samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
      train = logical(nrow(dayset))
      train[samples] = TRUE
      test = !train
      dayset.train = dayset[train,]
      dayset.test = dayset[test,]
    }
    
    
    ################## Place model Below
    
    ### Linear Regression
    {
      model = lm(cnt ~
                               + spring + summer + fall + winter
                               + spring_summer + summer_fall + fall_winter + winter_spring
                               + january + february + march + april + may + june + july + august + september + october + november + december
                               + sunday + monday + tuesday + wednesday + thursday + friday + saturday
                               + holiday + weekday + workingday
                               
                               + clear + mist + light_snow_rain + heavy_rain
                               + temp + atemp + hum + windspeed
                               ,data=dayset, subset=train)
      
    ##################
      
      
      ## Training error
      predictions.train = predict(model, newdata=dayset.train)
      YTrain = dayset.train$cnt
      train_error = mean((YTrain - predictions.train)^2)
      
    
      ## Test error (cross validation)
      predictions.test = predict(model, newdata=dayset.test)
      YTest = dayset.test$cnt
      test_error = mean((YTest - predictions.test)^2)
      
      
      # Plot and save image to file
      png(paste("LinearReg_TimeCat_Weather_trial_", trial, ".png", sep=""))
      plot(dayset$instant, dayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
      lines(dayset.train$instant, predictions.train, col="blue")
      points(dayset.test$instant, predictions.test, col="red", lwd=3)
      dev.off()
      
      # save test and train error results for each trial
      trials_results = rbind(trials_results, cbind(trial, seed, train_error, test_error))
    }
  }
  
  trials_results_summary = data.frame(error_statistic=str(0), train=numeric(0), test=numeric(0))
  trials_results_summary = rbind(trials_results_summary,
    cbind('min', min(trials_results$train_error), min(trials_results$test_error)),
    cbind('max', max(trials_results$train_error), max(trials_results$test_error)),
    cbind('mean', mean(trials_results$train_error), mean(trials_results$test_error)),
    cbind('std dev', sd(trials_results$train_error), sd(trials_results$test_error)))
  
  
  ################## Change names of destinatio variables
  dayset.linear_reg.trials = trials_results
  dayset.linear_reg.trials.summary = trials_results_summary
  ################## 
}








### Best Subset
{
  dayset.model.best_subset = regsubsets(cnt ~
                                        season + 
                                        + spring + summer + fall + winter
                                        + spring_summer + summer_fall + fall_winter + winter_spring
                                        + january + february + march + april + may + june + july + august + september + october + november + december
                                        + sunday + monday + tuesday + wednesday + thursday + friday + saturday
                                        + holiday + weekday + workingday
                                        
                                        + clear + mist + light_snow_rain + heavy_rain
                                        + temp + atemp + hum + windspeed
                                  , data=dayset.train, nbest=1, nvmax=20)
  
  dayset.model.best_subset.summary = summary(dayset.model.best_subset)
  summary = summary(dayset.model.best_subset)
  
  
  
  
  cp_min = which.min(dayset.model.best_subset.summary$cp)
  adjr2_max = which.max(dayset.model.best_subset.summary$adjr2)
  bic_min = which.min(dayset.model.best_subset.summary$bic)
  
  par(mfrow=c(2,2))
  plot(dayset.model.best_subset.summary$cp, xlab="Predictors", ylab="Cp")
  points(cp_min, dayset.model.best_subset.summary$cp[cp_min], col="red", lwd=3)
  plot(dayset.model.best_subset.summary$adjr2, xlab="Predictors", ylab="Adjusted R^2")
  points(adjr2_max, dayset.model.best_subset.summary$adjr2[adjr2_max], col="red", lwd=3)
  plot(dayset.model.best_subset.summary$bic, xlab="Predictors", ylab="BIC")
  points(bic_min, dayset.model.best_subset.summary$bic[bic_min], col="red", lwd=3)
}


### Linear Regression Best Subset
### Random Train and Test Sets
{
  trials_results = data.frame(trial=integer(0), seed=integer(0), train_error=numeric(0), test_error=numeric(0))
  
  
  for (trial in 1:20)
  {
    seed = trial
    
    # Split data into train and test sets
    {
      set.seed(seed)
      
      train.size2=dim(dayset)[1] * 3/4
      samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
      train = logical(nrow(dayset))
      train[samples] = TRUE
      test = !train
      dayset.train = dayset[train,]
      dayset.test = dayset[test,]
    }
    
    
    ################## Place model Below
    
    ### Linear Regression
    {
      # Best Subset Predictors for BIC with 8 Predictors
      #model = lm(cnt ~ summer + winter + september + light_snow_rain + weekday + temp + hum + windspeed,
      #                                  data=dayset, subset=train)
      
      # Best Subset Predictors for Cp with 12 Predictors
      #model = lm(cnt ~ summer + winter
      #           + march + june + july + september
      #           + holiday + weekday
      #           + light_snow_rain + temp + hum + windspeed,
      #           data=dayset, subset=train)
      
      # Best Subset Predictors for R2 with 15 Predictors
      model = lm(cnt ~ season + fall_winter
                 + march + april + june + july + september + october
                 + holiday + weekday
                 + clear + light_snow_rain + temp + hum + windspeed,
                 data=dayset, subset=train)
      
      ##################
      
      
      ## Training error
      predictions.train = predict(model, newdata=dayset.train)
      YTrain = dayset.train$cnt
      train_error = mean((YTrain - predictions.train)^2)
      
      
      ## Test error (cross validation)
      predictions.test = predict(model, newdata=dayset.test)
      YTest = dayset.test$cnt
      test_error = mean((YTest - predictions.test)^2)
      
      
      # Plot and save image to file
      png(paste("LinearReg_BestSub_R2_trial_", trial, ".png", sep=""))
      plot(dayset$instant, dayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
      lines(dayset.train$instant, predictions.train, col="blue")
      points(dayset.test$instant, predictions.test, col="red", lwd=3)
      dev.off()
      
      # save test and train error results for each trial
      trials_results = rbind(trials_results, cbind(trial, seed, train_error, test_error))
    }
  }
  
  trials_results_summary = data.frame(error_statistic=str(0), train=numeric(0), test=numeric(0))
  trials_results_summary = rbind(trials_results_summary,
                                 cbind('min', min(trials_results$train_error), min(trials_results$test_error)),
                                 cbind('max', max(trials_results$train_error), max(trials_results$test_error)),
                                 cbind('mean', mean(trials_results$train_error), mean(trials_results$test_error)),
                                 cbind('std dev', sd(trials_results$train_error), sd(trials_results$test_error)))
  
  
  ################## Change names of destinatio variables
  dayset.linear_reg.best_subset.trials = trials_results
  dayset.linear_reg.best_subset.trials.summary = trials_results_summary
  ################## 
}





#time_prediction(dayset)

# Linear Model with line(time) + abs(sin(time))
#time_prediction <- function(dayset)
{
  print(nrow(dayset))
  
  # Divide dataset into test and training sets to test
  # if the model can predict future bikeshare cnt
  
  # the fraction of the time to use for training in order to make predictions for the remaining time
  train_ratio = 0.5
  train_size = round(nrow(dayset)*train_ratio)
  dayset.time_train = dayset[1:train_size,]
  dayset.time_test = dayset[(train_size+1):nrow(dayset),]

  
  
  
  par(mfrow=c(1,1))
  plot(dayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
  
  # assume 1 repetition of sine wave throughout the dataset
  #dayset.time_model = lm(cnt ~ instant + abs(sin( (instant/max(dayset$instant)) * 2*pi )),
  #                       data=dayset.time_train)
  
  #dayset.time_model = lm(cnt ~ instant + abs(sin( (instant/max(dayset$instant)) * 2*pi ))
  #                       + atemp + temp + hum + windspeed + holiday + clear + mist + light_snow_rain + heavy_rain,
  #                       data=dayset.time_train)

  
  dayset.time_model = lm(cnt ~ instant + abs(sin( (instant/max(dayset$instant)) * 2*pi ))
                         #+ spring + summer + fall + winter
                         #+ spring_summer + summer_fall + fall_winter + winter_spring
                         #+ january + february + march + april + may + june + july + august + september + october + november + december
                         #+ sunday + monday + tuesday + wednesday + thursday + friday + saturday
                         #+ holiday + weekday + workingday
    
                         + clear + mist + light_snow_rain + heavy_rain
                         + temp + atemp + hum + windspeed
    , data=dayset.time_train)
  
  
  # plot fit to training data
  predictions.train = predict(dayset.time_model, newdata=dayset.time_train)
  lines(dayset.time_train$instant, predictions.train, col="blue", lwd=6)
  
  # plot predictions on test data
  predictions.test = predict(dayset.time_model, newdata=dayset.time_test)
  points(dayset.time_test$instant, predictions.test, col="red")
  
  #legend(x=-10, y=8800, legend=c('Train','Test'), col=c('blue','red'), lty=1, lwd=3)
  
  
  # cross validation error
  YTest = dayset.time_test$cnt
  error = mean((YTest - predictions.test)^2)
  dayset.model.linear.test_error = error
  print(paste('Linear Model Test Error: ', dayset.model.linear.test_error))
}






### Linear Regression (with instant)
### Training Fraction adjustment
{
  total_trials = 40
  
  trials_results = data.frame(trial=integer(0), train_ratio=numeric(0), train_error=numeric(0), test_error=numeric(0))
  
  
  for (trial in 1:(total_trials-1))
  {
    seed = trial
    
    # Split data into train and test sets
    {
      set.seed(seed)
      
      #train.size2=dim(dayset)[1] * 3/4
      #samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
      #train = logical(nrow(dayset))
      #train[samples] = TRUE
      #test = !train
      #dayset.train = dayset[train,]
      #dayset.test = dayset[test,]
      
      
      train_ratio = trial/total_trials
      train_size = round(nrow(dayset)*train_ratio)
      dayset.time_train = dayset[1:train_size,]
      dayset.time_test = dayset[(train_size+1):nrow(dayset),]
    }
    
    
    ################## Place model Below
    
    ### Linear Regression
    {

      model = lm(cnt ~ instant + abs(sin( (instant/max(dayset$instant)) * 2*pi ))
                                #+ spring + summer + fall + winter
                                #+ spring_summer + summer_fall + fall_winter + winter_spring
                                #+ january + february + march + april + may + june + july + august + september + october + november + december
                                #+ sunday + monday + tuesday + wednesday + thursday + friday + saturday
                                #+ holiday + weekday + workingday
           
           + clear + mist + light_snow_rain + heavy_rain
           + temp + atemp + hum + windspeed
           , data=dayset.time_train)
      
      ##################
      
      
      ## Training error
      predictions.train = predict(model, newdata=dayset.time_train)
      YTrain = dayset.time_train$cnt
      train_error = mean((YTrain - predictions.train)^2)
      
      
      ## Test error (cross validation)
      predictions.test = predict(model, newdata=dayset.time_test)
      YTest = dayset.time_test$cnt
      test_error = mean((YTest - predictions.test)^2)
      
      
      # Plot and save image to file
      png(paste("LinearReg_Instant_TimeCat_trial_", trial, ".png", sep=""))
      plot(dayset$instant, dayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
      lines(dayset.time_train$instant, predictions.train, col="blue", lwd=6)
      points(dayset.time_test$instant, predictions.test, col="red", lwd=3)
      text(x=50, y=8000, labels=round(test_error/10000)/100, cex=2.0)
      dev.off()
      
      # save test and train error results for each trial
      trials_results = rbind(trials_results, cbind(trial, train_ratio, train_error, test_error))
    }
  }
  
  trials_results_summary = data.frame(error_statistic=str(0), train=numeric(0), test=numeric(0))
  trials_results_summary = rbind(trials_results_summary,
                                 cbind('min', min(trials_results$train_error), min(trials_results$test_error)),
                                 cbind('max', max(trials_results$train_error), max(trials_results$test_error)),
                                 cbind('mean', mean(trials_results$train_error), mean(trials_results$test_error)),
                                 cbind('std dev', sd(trials_results$train_error), sd(trials_results$test_error)))
  
  
  ################## Change names of destinatio variables
  dayset.linear_reg_instant.trials = trials_results
  dayset.linear_reg_instant.trials.summary = trials_results_summary
  ################## 
}







### Linear Regression (with instant and weather)
### Training Fraction adjustment
{
  total_trials = 20
  
  trials_results = data.frame(trial=integer(0), seed=integer(0), train_error=numeric(0), test_error=numeric(0))
  
  
  for (trial in 1:(total_trials))
  {
    seed = trial
    
    # Split data into train and test sets
    {
      set.seed(seed)
      
      #train.size2=dim(dayset)[1] * 3/4
      #samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
      #train = logical(nrow(dayset))
      #train[samples] = TRUE
      #test = !train
      #dayset.train = dayset[train,]
      #dayset.test = dayset[test,]
      
    
      train_ratio = 0.5
      train_size = round(nrow(dayset)*train_ratio)
      dayset.time_train = dayset[1:train_size,]
      dayset.time_test = dayset[(train_size+1):nrow(dayset),]
      
      samples = sample(1:nrow(dayset.time_train), round(train_size * 3/4), replace=FALSE)
      dayset.time_train = dayset.time_train[samples,]
      
      samples = sample(1:nrow(dayset.time_test), round(train_size * 3/4), replace=FALSE)
      dayset.time_test = dayset.time_test[samples,]
      
      
    }
    
    
    ################## Place model Below
    
    ### Linear Regression
    {
      
      model = lm(cnt ~ instant + abs(sin( (instant/max(dayset$instant)) * 2*pi ))
                 #+ spring + summer + fall + winter
                 #+ spring_summer + summer_fall + fall_winter + winter_spring
                 #+ january + february + march + april + may + june + july + august + september + october + november + december
                 #+ sunday + monday + tuesday + wednesday + thursday + friday + saturday
                 #+ holiday + weekday + workingday
                 
                 + clear + mist + light_snow_rain + heavy_rain
                 + temp + atemp + hum + windspeed
                 , data=dayset.time_train)
      
      ##################
      
      
      ## Training error
      predictions.train = predict(model, newdata=dayset.time_train)
      YTrain = dayset.time_train$cnt
      train_error = mean((YTrain - predictions.train)^2)
      
      
      ## Test error (cross validation)
      predictions.test = predict(model, newdata=dayset.time_test)
      YTest = dayset.time_test$cnt
      test_error = mean((YTest - predictions.test)^2)
      
      
      # Plot and save image to file
      png(paste("LinearReg_Instant_Weather_trial_", trial, ".png", sep=""))
      plot(dayset$instant, dayset$cnt, xlab="Time (Days)", ylab="Bikeshare Users")
      points(dayset.time_train$instant, predictions.train, col="blue", lwd=3)
      points(dayset.time_test$instant, predictions.test, col="red", lwd=3)
      #text(x=50, y=8000, labels=round(test_error/10000)/100, cex=2.0)
      dev.off()
      
      # save test and train error results for each trial
      trials_results = rbind(trials_results, cbind(trial, seed, train_error, test_error))
    }
  }
  
  trials_results_summary = data.frame(error_statistic=str(0), train=numeric(0), test=numeric(0))
  trials_results_summary = rbind(trials_results_summary,
                                 cbind('min', min(trials_results$train_error), min(trials_results$test_error)),
                                 cbind('max', max(trials_results$train_error), max(trials_results$test_error)),
                                 cbind('mean', mean(trials_results$train_error), mean(trials_results$test_error)),
                                 cbind('std dev', sd(trials_results$train_error), sd(trials_results$test_error)))
  
  
  ################## Change names of destinatio variables
  dayset.linear_reg_instant.trials = trials_results
  dayset.linear_reg_instant.trials.summary = trials_results_summary
  ################## 
}

