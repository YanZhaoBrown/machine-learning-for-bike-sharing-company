dayset = read.csv("day.csv")
attach(dayset)

### Encode Categorical Variables in Multiple Columns
# Predictors to split: season, mnth, weekday, weathersit

{
  
  # Split Season
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
  
  
  
  # Split Weather Situation
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
  
}

dayset$dteday =NULL



### Split data into Test and Training sets
{
  set.seed(1)
  
  #train.size1=dim(hourset)[1] * 3/4
  #samples = sample(1:nrow(hourset), train.size1, replace=FALSE)
  #train = logical(nrow(hourset))
  #train[samples] = TRUE
  #test = !train
  #hourset.train = hourset[train,]
  #hourset.test = hourset[test,]
  
  train.size2=dim(dayset)[1] * 3/4
  samples = sample(1:nrow(dayset), train.size2, replace=FALSE)
  train = logical(nrow(dayset))
  train[samples] = TRUE
  test = !train
  dayset.train = dayset[train,]
  dayset.test = dayset[test,]
  
}




{na.omit(dayset)
  na.omit(dayset.test)
  na.omit(dayset.train)
  
  dayset = data.frame(dayset)
  dayset.train = data.frame(dayset.train)
  dayset.test = data.frame(dayset.test)}

names(dayset)
#[1] "s1"              "s2"              "sunday"          "monday"          "tuesday"         "wednesday"       "thursday"       
#[8] "friday"          "saturday"        "january"         "february"        "march"           "april"           "may"            
#[15] "june"            "july"            "august"          "september"       "october"         "november"        "december"       
#[22] "clear"           "mist"            "light_snow_rain" "heavy_rain"      "spring"          "summer"          "fall"           
#[29] "winter"          "instant"         "season"          "yr"              "mnth"            "holiday"         "weekday"        
#[36] "workingday"      "weathersit"      "temp"            "atemp"           "hum"             "windspeed"       "casual"         
#[43] "registered"      "cnt"   






