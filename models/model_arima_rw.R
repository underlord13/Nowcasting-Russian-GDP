#install.packages("forecast")
#install.packages("stats")
library("forecast")
library("stats")
library("reshape2")


setwd("C:/Users/lysov/Desktop/4 курс/ВКР/New Data")

# Data Setup --------------------------------------------------------------

metadata <- read.csv("meta_data.csv", dec = ',', sep = ";")
data <- read.csv("data_tf.csv", dec = ',', sep = ";") 

data$date <- as.Date(data$date, format = "%d.%m.%Y")
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

train_start_date <- "2011-01-01"
train_end_date <- "2018-12-01"
test_start_date <- "2019-03-01"
test_end_date <- "2023-09-01"

sectors <- c('agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')

lags <- -2:2

for (col in colnames(data)) {
  if (sum(is.infinite(data[,col])) > 0) {
    data[is.infinite(data[,col]), col] <- NA 
  }
}

data <- data %>% select(c("date", "gdp", all_of(sectors)))

# train and test datasets
train <- data %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  data.frame()
test <- data %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>%
  data.frame()

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")

# GDP ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}


target_variable <- "gdp"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}


 # for (i in 1:length(test_dates)){
 #   train <- test %>%
 #     dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  #data as it would have appeared at beginning of prediction period
 # 
 #   arima_predictions[arima_predictions$date == test_dates[i], "prediction"] <- as.numeric(as.data.frame(forecast(Arima(train[, target_variable], order = c(2, 0, 0)), h = 1))$`Point Forecast`)
 # }

 # for (i in 1:length(test_dates)){
 #   train <- test %>%
 #     dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2] & date >= seq(as.Date(test_dates[i]), by="-3 months", length=21)[21])  #data as it would have appeared at beginning of prediction period
 # 
 #   arima_predictions[arima_predictions$date == test_dates[i], "prediction"] <- as.numeric(as.data.frame(forecast(auto.arima(train[, target_variable]), h = 1))$`Point Forecast`)
 # 
 # }


actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))


data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_gdp_arima.xlsx")


# agri ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "agri"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_agri_arima.xlsx")

# mining ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "mining"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_mining_arima.xlsx")

# manufacturing ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "manufacturing"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_manufacturing_arima.xlsx")

# egpiv ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "egpiv"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_egpiv_arima.xlsx")

# construction ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "construction"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_construction_arima.xlsx")


# trade ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "trade"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_trade_arima.xlsx")

# transport ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "transport"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_transport_arima.xlsx")

# finance ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "finance"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_finance_arima.xlsx")

# real_estate ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "real_estate"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_real_estate_arima.xlsx")

# others ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "others"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_others_arima.xlsx")

# taxes ---------------------------------------------------------------------
test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
arima_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  arima_predictions[,as.character(lag)] <- NA
}

target_variable <- "taxes"

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(Arima(lagged_data[, target_variable], order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML", optim.method = "L-BFGS-B"), h = 1))$`Point Forecast`)
    arima_predictions[arima_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()

sqrt(mean((actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"])**2))
mean(abs(actuals - arima_predictions[1:(nrow(arima_predictions)), "-2"]))

data_graph_arima <- data.frame(date = as.Date(test_dates),
                               two_back = arima_predictions[, "-2"],
                               one_back = arima_predictions[, "-1"],
                               zero_back = arima_predictions[, "0"],
                               one_ahead = arima_predictions[, "1"],
                               two_ahead = arima_predictions[, "2"],
                               actuals = c(actuals))

data_graph_arima_long <- melt(data_graph_arima, id = "date") 

ggplot(data = data_graph_arima_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)


openxlsx::write.xlsx(data_graph_arima, "predictions_taxes_arima.xlsx")






# Random walk GDP ---------------------------------------------------------
target_variable <- "gdp"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                               two_back = rw_predictions[, "-2"],
                               one_back = rw_predictions[, "-1"],
                               zero_back = rw_predictions[, "0"],
                               one_ahead = rw_predictions[, "1"],
                               two_ahead = rw_predictions[, "2"],
                               actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_gdp_rw.xlsx")




# Random walk Agri ---------------------------------------------------------
target_variable <- "agri"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_agri_rw.xlsx")

# Random walk Mining ---------------------------------------------------------
target_variable <- "mining"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_mining_rw.xlsx")

# Random walk Manufacturing ---------------------------------------------------------
target_variable <- "manufacturing"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_manufacturing_rw.xlsx")

# Random walk EGPiV ---------------------------------------------------------
target_variable <- "egpiv"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_egpiv_rw.xlsx")

# Random walk Construction ---------------------------------------------------------
target_variable <- "construction"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_construction_rw.xlsx")

# Random walk Trade ---------------------------------------------------------
target_variable <- "trade"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_trade_rw.xlsx")

# Random walk Transport ---------------------------------------------------------
target_variable <- "transport"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_transport_rw.xlsx")

# Random walk Finance ---------------------------------------------------------
target_variable <- "finance"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_finance_rw.xlsx")

# Random walk Real Estate ---------------------------------------------------------
target_variable <- "real_estate"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_real_estate_rw.xlsx")

# Random walk Others ---------------------------------------------------------
target_variable <- "others"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_others_rw.xlsx")


# Random walk Taxes ---------------------------------------------------------
target_variable <- "taxes"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
rw_predictions <- data.frame(date = test_dates)
for (lag in lags) {
  rw_predictions[,as.character(lag)] <- NA
}

for (i in 1:length(test_dates)){
  train <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2])  
  
  for (lag in lags){
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
     
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- as.numeric(as.data.frame(forecast(rwf(train[, target_variable], h = 1, drift = FALSE)))$`Point Forecast`)
    rw_predictions[rw_predictions$date == test_dates[i], as.character(lag)] <- prediction
  }
}

actuals <- test %>%
  dplyr::filter(date >= test_start_date) %>%
  select(!!target_variable) %>%
  dplyr::filter(!is.na(!!sym(target_variable))) %>%
  pull()


sqrt(mean((actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"])**2))
mean(abs(actuals - rw_predictions[1:(nrow(rw_predictions)), "-2"]))

data_graph_rw <- data.frame(date = as.Date(test_dates),
                            two_back = rw_predictions[, "-2"],
                            one_back = rw_predictions[, "-1"],
                            zero_back = rw_predictions[, "0"],
                            one_ahead = rw_predictions[, "1"],
                            two_ahead = rw_predictions[, "2"],
                            actuals = c(actuals))

data_graph_rw_long <- melt(data_graph_rw, id = "date") 

ggplot(data = data_graph_rw_long,
       aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.5) + theme_bw(base_size = 20) + geom_point() +
  geom_text(aes(label = ifelse(date > as.Date('2023-12-01'), round(value, 2), ''),
                vjust = -1, hjust = 0.5)) + 
  geom_hline(yintercept = 0, size = 0.5, alpha = 0.5)

openxlsx::write.xlsx(data_graph_rw, "predictions_taxes_rw.xlsx")

