# Functions used for processing data and producing plots
# Used throughout the report


# Loads the csv data from path into a dataframe
# Input: 
#   (str) data path
data_prep <- function(path){
  data <- read.csv(path, header=TRUE)
  by_day <- as.data.frame(table(data$date))
  # Convert the data into a date-type
  by_day$date <- as.Date(by_day$Var1)
  return(by_day)
  }
date_check <- function(df, date){
}

# Pipeline for loading data from multiple files
# Input:
#   (array) array of strings containing data paths
#   (names) array of strings containing names for labelling data (city names)
#   (folder_path) a string containing the path to folder with data
#   (date1) a string of first date for interval in format (YYYY-MM-DD)
#   (date2) a string of the second date for interval in format (YYYY-MM-DD)
multi_data <- function(array, 
                       names, 
                       folder_path, 
                       date1, 
                       date2){
  
  main <- date_select(date1, 
                      date2, 
                      data_prep(file.path(folder_path, array[1])))
  print(main)
  names(main)[2] <- names[1]
  for (i in c(2:length(names))){
    tmp <- date_select(date1, 
                       date2, 
                       data_prep(file.path(folder_path, array[i])))
    
    tmp <- fill_dates(tmp, date1)
    print(tmp)
    main[names[i]] <- tmp
    print("HERE")
  }
  
  return(main)
}

# Converts dates into date-type variables
fill_dates <- function(df, start_date){
  t = c()
  date_tally = as.Date(start_date)
  i = 1
  while (i <= nrow(df)){
    if (date_tally == df$date[i]){
      t <- append(t, df$Freq[i], length(t))
      } 
    else{
      while (date_tally != df$date[i]){
        t <- append(t, 0, length(t))
        date_tally = date_tally + 1
        }
      }
    date_tally = date_tally + 1
    i = i + 1
  }
  return(t)
  }


# Selects dataframe rows within date1 - date2 interval
# Input:
#   (date1) a string of first date for interval in format (YYYY-MM-DD)
#   (date2) a string of the second date for interval in format (YYYY-MM-DD)
#   (data) dataframe containing the data to be processed
date_select <- function(date1, date2, data){
  out <- data %>%
    filter(date >= as.Date(date1, "%Y-%m-%d") & date <= as.Date(date2, "%Y-%m-%d"))
  return(out)
}

# Prediction function for data beyond 2020-03-10, following lockdown
# Input:
#   (train_data) 
april_predict <- function(train_data, 
                          true_data, 
                          name){
  
  model <- tbats(ts(train_data[6:(length(train_data)-6)]))
  pred <- forecast(model, 
                   h=30)
  
  pred_val <- tail(pred$mean, 1)[1]     
  if (pred_val < 0){
    pred_val <- pred_val * -1
  }
  true_val <- true_data[length(true_data)-31]
  
  print(pred_val)
  
  percentage_loss = (pred_val - true_val) / pred_val
  
  df <- data.frame(city=c(name), 
                   perc=c(percentage_loss*-100))
  
  return(df)
}


# Compare data predictions against observations, computes percetnage change
compare_data <- function(datapath, names){
  main = data.frame(city=c(), perc=c())
  for (i in c(1:length(datapath))){
    city_data <- data_prep(datapath[i])
    true_data <- ma(date_select("2017-01-01", "2020-05-10", city_data)$Freq, 10)
    train_data <- ma(date_select("2017-01-01", "2020-03-10", city_data)$Freq, 10)
    
    city_df <- april_predict(train_data, true_data, names[i])
    
    main <- rbind(main, city_df)
  }
  return (main)
}


# Comparison of frequency against the previous year, computes percentage change
prev_year_comp <- function(path){
  data <- data_prep(path)
  a = train_data <- ma(date_select("2019-01-01", 
                                   "2021-01-01", 
                                   data)$Freq, 10)
  a_2020 = ts(a[365:length(a)])
  a_2019 = ts(a[0:365])
  
  diff = (a_2019[213] - a_2020[213]) / (a_2019[213])
  
  return(diff)
}

# Iterates the above function over array of paths
multi_prev_year <- function(paths, names){
  main = data.frame(city=c(), perc=c())
  for (i in c(1:length(paths))){
    tmp = data.frame(city=c(names[i]), 
                     perc=c(prev_year_comp(paths[i])))
    main = rbind(main, tmp)
  }
  
  return(main)
}

# Producees bar plot displaying the percentage change from the above functions
percentage_plot <- function(df){
  ggplot(df, aes(x=city, y=perc)) + 
    geom_bar(stat="identity", fill="#FF6666")  + 
    scale_x_discrete(position = "top") + 
    xlab("City") + 
    ylab("Percentage change from March to Arpil (%)")
}