library(tidyverse)
library(midasr)
library(imputeTS)

gen_lagged_data <- function (metadata, data, last_date, lag) {
  lagged_data <- data %>% 
    dplyr::filter(date <= last_date)
  
  for (col in colnames(lagged_data)[2:length(colnames(lagged_data))]) {
    pub_lag <- metadata %>% 
      dplyr::filter(series == col) %>% 
      select(months_lag) %>% 
      pull()
    
    condition <- (nrow(lagged_data) - pub_lag + lag)
    if (condition <= nrow(lagged_data)) {
      lagged_data[condition:nrow(lagged_data), col] <- NA
    } 
  }
  lagged_data <- lagged_data %>% 
    dplyr::filter(!is.na(date))
  return (lagged_data)
}

setwd("C:/Users/lysov/Desktop/4 курс/ВКР/New Data")

# Data Setup --------------------------------------------------------------
metadata <- read.csv("meta_data.csv", dec = ',', sep = ";")
data <- read.csv("data_tf.csv", dec = ',', sep = ";") 

library(readxl)
data <- read_excel("data_tf.xlsx")

data$date <- as.Date(data$date, format = "%d.%m.%Y")

train_start_date <- "2011-01-01"
train_end_date <- "2018-12-01"
test_start_date <- "2019-03-01"
test_end_date <- "2023-09-01"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
lags = -2:2
# quart variables
quarts <- metadata$series[metadata$freq == 'q']

library("psych")
sectors <- c('gdp', 'agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')

cor_test_mat <- corr.test(data[1:nrow(data),2:ncol(data)],
                          use = "na.or.complete", method="pearson")$p 
cor_test_mat <- cor_test_mat[,c(sectors)] 
cor_test_mat <- as.data.frame(cor_test_mat)

# Grid-search p-val

evaluate_midas <- function(p_value, sector, save) {
  target_variable <- sector
  cor_test_mat_sector <- c(cor_test_mat[target_variable] < p_value)
  data_t<-data[2:ncol(data)]
  data2<-data_t[cor_test_mat_sector]
  data_sector<-cbind(data[1], data2)
  if(target_variable != 'gdp'){
    data_sector$gdp<-NULL #Remove GDP
  } else{
    data_sector[, sectors[2:length(sectors)]]  <- NULL #Remove GDP
  }
  
  test <- data_sector %>%
    dplyr::filter(date >= train_start_date, date <= test_end_date) %>%
    data.frame()
  
  # dataframe for predictions
  pred_dict <- data.frame(date = test_dates)
  for (lag in lags) {
    pred_dict[,as.character(lag)] <- NA
  }
  
  # looping through test dates
  for (i in 1:length(test_dates)) {
    # training the actual model
    train <- test %>%
      dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) %>% # data as it would have appeared at beginning of prediction period
      na_mean()
    
    # estimating MIDAS models
    y <- train[substr(train$date, 6, 7) %in% c("03", "06", "09", "12"),  target_variable]
    models <- list()
    
    # for MIDAS, a separate model is trained for each explanatory variable
    for (col in colnames(train)[2:ncol(train)]) {
      if (col != target_variable) {
        if (col %in% quarts) { # quarterly variables
          x <- train[substr(train$date, 6, 7) %in% c("03", "06", "09", "12"), col]
          models[[col]] <- midas_r(y ~ mls(x, 0:1, 1, nealmon), start=list(x=c(1, -0.5)))
        } else { # monthly variables, the mls() function handles lagging and flattening the data
          x <- train[,col]
          models[[col]] <- midas_r(y ~ mls(x, 0:3, 3, nealmon), start=list(x=c(1, -0.5)))
        } 
      }
    }
    
    # some variables have more predictive power than others, a straight average weighting would give too much weight to those unhelpful variables
    # RMSE for weighting based on fit on the training set gives much better results
    weight <- list()
    for (col in colnames(train)[2:ncol(train)]) {
      if (col != target_variable) {
        fitted <- models[[col]]$fitted.values
        actual <- y[2:length(y)]
        weight[[col]] <- (fitted - actual)**2 %>% mean() %>% sqrt()
      }
    }
    # discount the least predictive variable
    adj <- abs(unlist(weight) - max(unlist(weight)))
    weight <- (adj / sum(adj))
    
    
    # testing the model on artificial vintages  
    for (lag in lags) {
      lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
        data.frame
      # make sure actual value not in there
      lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
      lagged_data <- na_mean(lagged_data) 
      
      y <- lagged_data[substr(lagged_data$date, 6, 7) %in% c("03", "06", "09", "12"),  target_variable]
      preds <- list()
      
      # loop through all the individual models to get predictions, later will weighted average them
      for (col in colnames(test)[2:ncol(test)]) {
        if (col != target_variable) {
          if (col %in% quarts) { # quarterly variables
            x <- lagged_data[substr(lagged_data$date, 6, 7) %in% c("03", "06", "09", "12"), col]
            preds[[col]] <- forecast(models[[col]], newdata=list(x=x))$mean
            preds[[col]] <- preds[[col]][length(preds[[col]])]
          } else { # monthly variables
            x <- lagged_data[,col]
            preds[[col]] <- forecast(models[[col]], newdata=list(x=x))$mean
            preds[[col]] <- preds[[col]][length(preds[[col]])]
          } 
        }
      }
      # averaged predictions
      pred_dict[pred_dict$date == test_dates[i], as.character(lag)] <- weighted.mean(unlist(preds), weight)
    }
  }
  
  actuals <- test %>%
    dplyr::filter(date >= test_start_date) %>%
    dplyr::filter(substr(date, 6, 7) %in% c("03", "06", "09", "12")) %>%
    select(!!target_variable) %>%
    pull()
  
  midas_predictions <- pred_dict[, c("-2", "-1", "0", "1", "2")]
  
  performance <- data.frame(Vintage = numeric(), RMSE = numeric())
  for (lag in lags) {
    tmp = data.frame(
      Vintage = lag,
      RMSE = sqrt(mean((actuals - pred_dict[,as.character(lag)])**2)),
      MAE = mean(abs(actuals - pred_dict[1:(nrow(pred_dict)),as.character(lag)]))
    )
    performance = rbind(performance, tmp)
  }
  round(performance, 4)
  
  data_graph_midas <- data.frame(date = as.Date(test_dates),
                                 predictions = midas_predictions,
                                 actuals = c(actuals))
  data_graph_midas_long <- melt(data_graph_midas, id = "date") 
  
  ggplot(data = data_graph_midas_long,
         aes(x = date, y = value, color = variable)) +
    geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
    geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                  vjust = -1, hjust = 0.5)) + 
    geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)
  
  if (save == 1){
    openxlsx::write.xlsx(data_graph_midas, sprintf("predictions_%s_midas.xlsx", sector))
  }
}

try_midas <- function(p_value, sector) {
  tryCatch(
    {
      target_variable <- sector
      cor_test_mat_sector <- c(cor_test_mat[target_variable] < p_value)
      data_t<-data[2:ncol(data)]
      data2<-data_t[cor_test_mat_sector]
      data_sector<-cbind(data[1], data2)
      if(target_variable != 'gdp'){
        data_sector$gdp<-NULL #Remove GDP
      } else{
        data_sector[, sectors[2:length(sectors)]]  <- NULL #Remove GDP
      }
      
      test <- data_sector %>%
        dplyr::filter(date >= train_start_date, date <= test_end_date) %>%
        data.frame()
      
      # dataframe for predictions
      pred_dict <- data.frame(date = test_dates)
      for (lag in lags) {
        pred_dict[,as.character(lag)] <- NA
      }
      
      # looping through test dates
      for (i in 1:length(test_dates)) {
        # training the actual model
        train <- test %>%
          dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) %>% # data as it would have appeared at beginning of prediction period
          na_mean()
        
        # estimating MIDAS models
        y <- train[substr(train$date, 6, 7) %in% c("03", "06", "09", "12"),  target_variable]
        models <- list()
        
        # for MIDAS, a separate model is trained for each explanatory variable
        for (col in colnames(train)[2:ncol(train)]) {
          if (col != target_variable) {
            if (col %in% quarts) { # quarterly variables
              x <- train[substr(train$date, 6, 7) %in% c("03", "06", "09", "12"), col]
              models[[col]] <- midas_r(y ~ mls(x, 0:1, 1, nealmon), start=list(x=c(1, -0.5)))
            } else { # monthly variables, the mls() function handles lagging and flattening the data
              x <- train[,col]
              models[[col]] <- midas_r(y ~ mls(x, 0:3, 3, nealmon), start=list(x=c(1, -0.5)))
            } 
          }
        }
        
        # some variables have more predictive power than others, a straight average weighting would give too much weight to those unhelpful variables
        # RMSE for weighting based on fit on the training set gives much better results
        weight <- list()
        for (col in colnames(train)[2:ncol(train)]) {
          if (col != target_variable) {
            fitted <- models[[col]]$fitted.values
            actual <- y[2:length(y)]
            weight[[col]] <- (fitted - actual)**2 %>% mean() %>% sqrt()
          }
        }
        # discount the least predictive variable
        adj <- abs(unlist(weight) - max(unlist(weight)))
        weight <- (adj / sum(adj))
        
        
        # testing the model on artificial vintages  
        for (lag in lags) {
          lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
            data.frame
          # make sure actual value not in there
          lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
          lagged_data <- na_mean(lagged_data) # fill missing with mean to get estimate
          
          y <- lagged_data[substr(lagged_data$date, 6, 7) %in% c("03", "06", "09", "12"),  target_variable]
          preds <- list()
          
          # loop through all the individual models to get predictions, later will weighted average them
          for (col in colnames(test)[2:ncol(test)]) {
            if (col != target_variable) {
              if (col %in% quarts) { # quarterly variables
                x <- lagged_data[substr(lagged_data$date, 6, 7) %in% c("03", "06", "09", "12"), col]
                preds[[col]] <- forecast(models[[col]], newdata=list(x=x))$mean
                preds[[col]] <- preds[[col]][length(preds[[col]])]
              } else { # monthly variables
                x <- lagged_data[,col]
                preds[[col]] <- forecast(models[[col]], newdata=list(x=x))$mean
                preds[[col]] <- preds[[col]][length(preds[[col]])]
              } 
            }
          }
          # averaged predictions
          pred_dict[pred_dict$date == test_dates[i], as.character(lag)] <- weighted.mean(unlist(preds), weight)
        }
      }
      

      actuals <- test %>%
        dplyr::filter(date >= test_start_date) %>%
        select(!!target_variable) %>%
        dplyr::filter(!is.na(!!sym(target_variable))) %>%
        pull()
      
      midas_predictions <- pred_dict[, "2"]
      
      MAE = mean(abs(actuals - midas_predictions))
      
      return(MAE)
    },
    error = function(error_message) {
      message('An Error Occurred')
      message(error_message)
      MAE <- 100000
      return(MAE)
    }
  )
}


# gdp ---------------------------------------------------------------------
library(pracma)
space <- linspace(1e-4, 2e-3, 40)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable <- 'gdp'

for (p_value in space) {
    curr_mae <- try_midas(p_value = p_value, sector = target_variable)
    
    if (curr_mae <= best_mae) {
      best_mae = curr_mae
      best_p_val = p_value
    }
    count <- (count + 1)
    message(sprintf('Iteration № %.0f', count))
    cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
    cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = 1, sector = target_variable, save = 0)

# agri --------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "agri"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable)

# agri
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "agri"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable)




# agri --------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "agri"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# mining ------------------------------------------------------------------
space <- linspace(1e-3, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "mining"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# manufacturing ------------------------------------------------------------------
space <- linspace(1e-3, 1, 20)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "manufacturing"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# egpiv ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "egpiv"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)


# construction ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "construction"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# trade ------------------------------------------------------------------
space <- linspace(1e-3, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "trade"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# transport ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "transport"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# finance ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "finance"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)

# real_estate ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "real_estate"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)


# others ------------------------------------------------------------------
space <- linspace(1e-2, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "others"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)


# taxes ------------------------------------------------------------------
space <- linspace(1e-3, 1, 30)
best_mae = 1000
best_p_val = NaN
count = 0

target_variable = "taxes"

for (p_value in space) {
  curr_mae <- try_midas(p_value = p_value, sector = target_variable)
  
  if (curr_mae <= best_mae) {
    best_mae = curr_mae
    best_p_val = p_value
  }
  count <- (count + 1)
  message(sprintf('Iteration № %.0f', count))
  cat(sprintf("Current MAE: %f. Params: p-value = %f.", curr_mae, p_value))
  cat(sprintf(" Best MAE: %f. Params: p-value = %f.", best_mae, best_p_val))
}

evaluate_midas(p_value = best_p_val, sector = target_variable, save = 1)


















