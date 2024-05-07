install.packages("devtools")
devtools::install_github("dhopp1/nowcastDFM")
ls()
setwd("C:/Users/lysov/Desktop/4 курс/ВКР/New Data")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("nowcastDFM")
library("dplyr")
library("tibble")
library("seasonal")
library("readxl")
library("psych")
library("reshape2")


# Data Setup --------------------------------------------------------------
metadata <- read.csv("meta_data.csv", dec = ',', sep = ";")
data <- read.csv("data_tf.csv", dec = ',', sep = ";") 

library(readxl)
data <- read_excel("data_tf.xlsx")

data$date <- as.Date(data$date, format = "%d.%m.%Y")

# target
target_variable <- "gdp"

# train-test split
train_start_date <- "2011-01-01"
train_end_date <- "2018-12-01"
test_start_date <- "2019-03-01"
test_end_date <- "2023-09-01"


# corr matrix
library("psych")
sectors <- c('gdp', 'agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')

cor_test_mat <- corr.test(data[1:nrow(data),2:ncol(data)],
                          use = "na.or.complete", method="pearson")$p 

cor_test_mat <- cor_test_mat[,c(sectors)]
cor_test_mat <- as.data.frame(cor_test_mat)
cor_test_mat['gdp']

# Function for DFM grid-search --------------------------------------------------
try_dfm <- function(p_value, p_lags, sector) {
  tryCatch(
    {
      cor_test_mat_sector <- c(cor_test_mat[sector] < p_value)
      data_t<-data[2:ncol(data)]
      data2<-data_t[cor_test_mat_sector]
      data_sector<-cbind(data[1], data2)
      if(target_variable != 'gdp'){
        data_sector$gdp<-NULL #Remove GDP
      } else{
        data_sector[, sectors[2:length(sectors)]]  <- NULL
      }
      colnames(data_sector)
      
      # train and test datasets
      train <- data_sector %>%
        dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
        data.frame()
      test <- data_sector %>%
        dplyr::filter(date >= train_start_date, date <= test_end_date) %>%
        data.frame()
      
      
      # getting blocks from metadata
      blocks <- metadata %>% 
        dplyr::filter(series %in% colnames(train))
      # order rows correctly
      blocks <- blocks[match(colnames(train[2:length(colnames(train))]), blocks$series),]
      blocks <- blocks %>% 
        dplyr::select(starts_with("block_")) %>%
        # drop blocks with no entries
        select_if(~sum(.) > 1) %>% 
        data.frame
      
      metadata_t <- metadata %>% 
        dplyr::filter(series %in% colnames(train))
      metadata_t <- metadata_t[match(colnames(train[2:length(colnames(train))]), metadata_t$series),]
      
      test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
      pred_dict <- data.frame(date = test_dates)
      for (lag in lags) {
        pred_dict[,as.character(lag)] <- NA
      }
      
      for (i in 1:length(test_dates)) {
        train_rolling <- test %>%
          dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
        output_dfm_sector <- dfm(data = train_rolling, p=p_lags, max_iter=100, threshold = 1e-10,
                              blocks = blocks)
        
        # testing the model on artificial vintages  
        
          lagged_data <- gen_lagged_data(metadata, test, test_dates[i], 2) %>% 
            data.frame
          # make sure actual value not in there
          lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
          prediction <- predict_dfm(lagged_data, output_dfm_sector) %>% 
            dplyr::filter(date == test_dates[i]) %>% 
            select(!!target_variable) %>% 
            pull()
          pred_dict[pred_dict$date == test_dates[i], as.character('pred')] <- prediction
      }
      
      actuals <- test %>%
        dplyr::filter(date >= test_start_date) %>%
        select(!!target_variable) %>%
        dplyr::filter(!is.na(!!sym(target_variable))) %>%
        pull()
      
      dfm_predictions <- pred_dict$pred
      
      MAE = mean(abs(actuals - dfm_predictions))

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


# Grid-search
actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

library(pracma)
space <- linspace(0.001, 0.02, 40)
p_lags <- c(1,2,3,4)
best_mae = 1000
best_p_val = NaN
best_p_lag = NaN
best_pred = NaN  
count = 0

for (p_value in space) {
  for (p_lag in p_lags) {
   curr_mae <- try_dfm(p_value = p_value, p_lags = p_lag, sector = target_variable)
    
    if (curr_mae <= best_mae) {
      best_mae = curr_mae
      best_p_val = p_value
      best_p_lag = p_lag
    }
    count <- (count + 1)
    message(sprintf('Iteration № %.0f', count))
    cat(sprintf("Current MAE: %f. Params: p-value = %f, lags = %.0f.", curr_mae, p_value, p_lag))
    cat(sprintf(" Best MAE: %f. Params: p-value = %f; lags = %.0f \n", best_mae, best_p_val, best_p_lag))
  }
}

best_mae #=0.653684
best_p_val #= 0.005
best_p_lag #= 1



# Prediction with optimal params ------------------------------------------

cor_test_mat_sector <- c(cor_test_mat[target_variable] < best_p_val)
data_t<-data[2:ncol(data)]
data2<-data_t[cor_test_mat_sector]
data_sector<-cbind(data[1], data2)
if(target_variable != 'gdp'){
  data_sector$gdp<-NULL #Remove GDP
} else{
  data_sector[, sectors[2:length(sectors)]]  <- NULL #Remove GDP
}
colnames(data_sector)

# train and test datasets
train <- data_sector %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_sector %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()


# getting blocks from metadata
blocks <- metadata %>% 
  dplyr::filter(series %in% colnames(train))
# order rows correctly
blocks <- blocks[match(colnames(train[2:length(colnames(train))]), blocks$series),]
blocks <- blocks %>% 
  dplyr::select(starts_with("block_")) %>%
  # drop blocks with no entries if happens with some variables
  select_if(~sum(.) > 1) %>% 
  data.frame

metadata_t <- metadata %>% 
  dplyr::filter(series %in% colnames(train))
metadata_t <- metadata_t[match(colnames(train[2:length(colnames(train))]), metadata_t$series),]

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
pred_dict <- data.frame(date = test_dates)
for (lag in lags) {
  pred_dict[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)) {
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_gdp <- dfm(data = train_rolling, p=best_p_lag, max_iter=150, threshold = 1e-10,
                        blocks = blocks)
  print(i)

  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_gdp) %>% 
      dplyr::filter(date == test_dates[i]) %>% 
      select(!!target_variable) %>% 
      pull()
    pred_dict[pred_dict$date == test_dates[i], as.character(lag)] <- prediction
    print(i)
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

dfm_predictions <- pred_dict[, c("-2", "-1", "0", "1", "2")]

performance <- data.frame(Vintage = numeric(), RMSE = numeric(), MAE = numeric())
for (lag in lags) {
  tmp = data.frame(
    Vintage = lag,
    RMSE = sqrt(mean((actuals - pred_dict[1:(nrow(pred_dict)),as.character(lag)])**2)),
    MAE = mean(abs(actuals - pred_dict[1:(nrow(pred_dict)),as.character(lag)]))
  )
  performance = rbind(performance, tmp)
}
round(performance, 4)

data_graph_dfm <- data.frame(date = as.Date(test_dates),
                             predictions = dfm_predictions,
                             actuals = c(actuals))
data_graph_dfm_long <- melt(data_graph_dfm, id = "date") 

ggplot(data = data_graph_dfm_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_dfm, "predictions_gdp_new.xlsx")

