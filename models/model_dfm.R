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


# Preparation -------------------------------------------------------------

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


#a <- read.csv("data_raw.csv", sep = ";", dec = ",")
#data <- read_excel("data_raw.xlsx") %>%
#  mutate(date = as.Date(date, format= "%d.%m.%Y")) %>% 
#  arrange(date) 
#%>% 
#  growth_rate()

#data$quart <- NULL
#data[, 2:(nrow(data) - 1)] <- sapply(data[, 2:(nrow(data) - 1)], as.numeric)

#gdp_vars <- c('gdp', 'agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
#             'transport', 'finance', 'real_estate', 'others', 'taxes')

#data[c('date', gdp_vars)] <- growth_rate(data[c('date', gdp_vars)])
#write_csv(data, "data_tf.csv")

# Data set up -------------------------------------------------------------

# full data read
metadata <- read.csv("meta_data.csv", dec = ',', sep = ";")
#data <- read.csv("data_tf.csv", dec = ',', sep = ";") 
data <- read_excel("data_tf.xlsx")
data$date <- as.Date(data$date, format = "%d.%m.%Y")

# which lags to assess
lags <- -2:2

train_start_date <- "2011-01-01"
train_end_date <- "2018-12-01"
test_start_date <- "2019-03-01"
test_end_date <- "2023-09-01"

test_dates <- seq(as.Date(test_start_date), as.Date(test_end_date), by = "3 months")
pred_dict <- data.frame(date = test_dates)
for (lag in lags) {
  pred_dict[,as.character(lag)] <- NA
}


sectors <- c('gdp', 'agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')

cor_test_mat <- corr.test(data[1:nrow(data),2:ncol(data)],
                          use = "na.or.complete", method="pearson")$p

cor_test_mat <- cor_test_mat[,c(sectors)]
cor_test_mat <- as.data.frame(cor_test_mat)

# GDP prediction ----------------------------------------------------------
target_variable <- "gdp"

sectors <- c('agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')


cor_test_mat_t <- cor_test_mat[, c("gdp")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("gdp")

cor_test_mat_gdp <- cor_test_mat_t$gdp < 0.005

data_t <- data[2:ncol(data)]

data2 <- data_t[cor_test_mat_gdp]

data_gdp <- cbind(data[1], data2)

#data_gdp <- data

data_gdp[, sectors] <- NULL #Remove sectors
data_gdp$PMI_Mnf <- data$PMI_Mnf
#############################################3

# train and test datasets
train <- data_gdp %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  data.frame()
test <- data_gdp %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()



# set.seed(123)
# filterCtrl <- sbfControl(functions = lmSBF, method = "boot", number = 5,
#                          verbose = TRUE)
# filterResults <- sbf(train[, -match(c("gdp", "date"), names(train))],
#                      train$gdp, sbfControl = filterCtrl, na.action = na.omit)
# filterResults$optVariables
# opt_vars <- filterResults$optVariables
# 
# # 
# rfeCtrl <- rfeControl(functions = lmFuncs, method = "boot", number = 5, verbose = TRUE,
#                       rerank = FALSE)
# rfeResults <- rfe(train[, -match(c("gdp", "date"), names(train))],
#                train$gdp, size = с(1:40), rfeControl = rfeCtrl, metric = 'MAE',
#                na.action = na.omit)
# plot(rfeResultsesults, type = c("g", "o"), metric = 'Rsquared')
# rfeResults$optVariables
# 
# opt_vars <- union(opt_vars, rfeResults$optVariables)
# opt_vars

# safsCtrl <- safsControl(functions = caretSA, method = 'boot', number = 5, verbose = TRUE)
# safsResults <- safs(x = train[2:nrow(train), -match(c("gdp", "date"), names(train))],
#                     y = train[2:nrow(train), 'gdp'], safsControl = safsCtrl, iters = 100, method = 'lm')

# opt_vars <- union(opt_vars, safsResults$optVariables)
# opt_vars

# data_gdp <- data_gdp %>% select("gdp", all_of(opt_vars))
# data_gdp <- cbind(data[1], data_gdp)


# train and test datasets
train <- data_gdp %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  data.frame()
test <- data_gdp %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% 
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
  output_dfm_gdp <- dfm(data = train_rolling, p=1, max_iter=20, threshold = 1e-10,
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


openxlsx::write.xlsx(data_graph_dfm, "predictions_gdp.xlsx")


# Mining prediction -------------------------------------------------------
target_variable <- "mining"

sectors <- c('agri', 'mining', 'manufacturing', 'egpiv', 'construction', 'trade',
             'transport', 'finance', 'real_estate', 'others', 'taxes')

cor_test_mat_t <- cor_test_mat[, c("mining")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("mining")

cor_test_mat_mining <- cor_test_mat_t$mining < 0.004

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_mining]
#factors_mining <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_mining]
data_m <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_m$gdp<-NULL #Remove GDP
data_m$ism_serv <- NULL
data_m$retail_prod <- NULL
data_m$obschepit <- NULL
data_m$IBC_trade_fact <- NULL
data_m$IBC_trade_demand <- NULL
#data_m$IBC_trade_credit <- NULL
#data_m$IBC_trade_output <- NULL

data_m$ip <- data$ip
data_m$ip_raw <- data$ip_raw
data_m$ip_oil_products <- data$ip_oil_products
data_m$ip_coal <- data$ip_coal
data_m$ip_metals <- data$ip_metals
data_m$ip_nonfood <- data$ip_nonfood
data_m$cargo_pipe <- data$cargo_pipe
data_m$constr_prices <- data$constr_prices


data_m[, setdiff(intersect(colnames(data_m), sectors), 'mining')] <- data_m %>%
  select(setdiff(intersect(colnames(data_m), sectors), 'mining')) %>%  as.null

# train and test datasets
train <- data_m %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  data.frame()
test <- data_m %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

# getting blocks from metadata
blocks <- metadata %>% 
  dplyr::filter(series %in% colnames(train))
# order rows correctly
blocks <- blocks[match(colnames(train[2:length(colnames(train))]), blocks$series),]
blocks <- blocks %>% 
  dplyr::select(starts_with("block_")) %>%
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
  output_dfm_m <- dfm(data = train_rolling, p=1, max_iter=20, threshold = 1e-10,
                      blocks = blocks)
  print(i)
  
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_m) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_mining_dfm.xlsx")

predictions_mining<-openxlsx::read.xlsx("predictions_mining_dfm.xlsx")

# Agriculture prediction --------------------------------------------------

target_variable <- "agri"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("agri")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("agri")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_agri <- cor_test_mat_t$agri < 0.05

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_agri]
#factors_mining <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_mining]
data_a <- cbind(data[1], data2)
data_a$gdp<-NULL #Remove GDP
data_a$gold <- NULL
data_a$IBC_mnf_output <- NULL
data_a$selhoz
colnames(data_a)

data_a[, setdiff(intersect(colnames(data_a), sectors), 'agri')] <- data_a %>%
  select(setdiff(intersect(colnames(data_a), sectors), 'agri')) %>%  as.null

# train and test datasets
train <- data_a %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_a %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_a <- dfm(data = train_rolling, p=1, max_iter=100, threshold = 1e-10,
                      blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_a) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_agri_dfm.xlsx")

predictions_agri<-openxlsx::read.xlsx("predictions_agri_dfm.xlsx")

# EGPIV prediction --------------------------------------------------------

target_variable <- "egpiv"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("egpiv")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("egpiv")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_egpiv <- cor_test_mat_t$egpiv < 0.055

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_egpiv]
#factors_mining <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_mining]
data_e <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_e$gdp<-NULL #Remove GDP
data_e[, setdiff(intersect(colnames(data_e), sectors), 'egpiv')] <- data_e %>%
  select(setdiff(intersect(colnames(data_e), sectors), 'egpiv')) %>%  as.null
data_e$mining<-predictions_mining$mining


# train and test datasets
train <- data_e %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_e %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_e <- dfm(data = train_rolling, p=2, max_iter=100, threshold = 1e-10,
                      blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_e) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_egpiv_dfm.xlsx")

predictions_egpiv<-openxlsx::read.xlsx("predictions_egpiv_dfm.xlsx")


# Transport prediction -----------------------------------------------------
target_variable <- "transport"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("transport")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("transport")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_transport <- cor_test_mat_t$transport < 0.01


data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_transport]
#factors_transport <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_transport]
data_trans <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_trans$gdp<-NULL #Remove GDP
#data_trans$ip <- data$ip
#data_trans$ip_raw <- data$ip_raw
data_trans$ip_oil_products <- data$ip_oil_products
data_trans$ip_metals <- data$ip_metals
data_trans$ip_man <- data$ip_man
data_trans$ip_food <- data$ip_food
data_trans$ip_nonfood <- data$ip_nonfood
data_trans$ip_el <- data$ip_el
data_trans$cargo_auto <- data$cargo_auto
data_trans$cargo_pipe <- data$cargo_pipe
data_trans$cargo_rail <- data$cargo_rail
data_trans$cargo_sea <- data$cargo_sea
data_trans$rail <- data$rail
data_trans$cpi_rail <- data$cpi_rail
data_trans$ism_serv <- NULL

data_trans[, setdiff(intersect(colnames(data_trans), sectors), 'transport')] <- data_trans %>%
  select(setdiff(intersect(colnames(data_trans), sectors), 'transport')) %>%  as.null

data_trans$mining<-predictions_mining$mining

# train and test datasets
train <- data_trans %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_trans %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_transp <- dfm(data = train_rolling, p=1, max_iter=20, threshold = 1e-10,
                           blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_transp) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_transport.xlsx")

predictions_transport<-openxlsx::read.xlsx("predictions_transport.xlsx")



# Trade prediction -----------------------------------------------------
target_variable <- "trade"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("trade")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("trade")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_trade <- cor_test_mat_t$trade < 0.02 #0.04

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_trade]
#factors_trade <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_trade]
data_trade <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_trade$gdp<-NULL #Remove GDP
data_trade$opt <- data$opt

data_trade[, setdiff(intersect(colnames(data_trade), sectors), 'trade')] <- data_trade %>%
  select(setdiff(intersect(colnames(data_trade), sectors), 'trade')) %>%  as.null

#data_trade$transport<-predictions_transport$transport
data_trade$mining<-predictions_mining$mining
data_trade$agri<-predictions_agri$agri

# train and test datasets
train <- data_trade %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_trade %>%
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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_trade <- dfm(data = train_rolling, p=1, max_iter=20, threshold = 1e-10,
                          blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_trade) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_trade_dfm.xlsx")

predictions_trade<-openxlsx::read.xlsx("predictions_trade_dfm.xlsx")


# Manufacturing prediction -----------------------------------------------------
target_variable <- "manufacturing"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("manufacturing")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("manufacturing")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_manufacturing <- cor_test_mat_t$manufacturing < 0.1

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_manufacturing]
#factors_manufacturing <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_manufacturing]
data_manufacturing <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_manufacturing$gdp<-NULL #Remove GDP
data_manufacturing$ip_man <- data$ip_man

data_manufacturing[, setdiff(intersect(colnames(data_manufacturing), sectors), 'manufacturing')] <- data_manufacturing %>%
  select(setdiff(intersect(colnames(data_manufacturing), sectors), 'manufacturing')) %>%  as.null

data_manufacturing$transport<-predictions_transport$transport
data_manufacturing$trade<-predictions_trade$trade
data_manufacturing$mining<-predictions_mining$mining
data_manufacturing$agri<-predictions_agri$agri

# train and test datasets
train <- data_manufacturing %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_manufacturing %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_manufacturing <- dfm(data = train_rolling, p=2, max_iter=20, threshold = 1e-10,
                                  blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_manufacturing) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_manufacturing_dfm.xlsx")

predictions_manufacturing<-openxlsx::read.xlsx("predictions_manufacturing.xlsx")



# Construction prediction -----------------------------------------------------
target_variable <- "construction"

# Apply corr.test function
cor_test_mat <- corr.test(data[1:nrow(data),
                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat <- cor_test_mat[, c("construction")]

cor_test_mat <- as.data.frame(cor_test_mat)
colnames(cor_test_mat) <- c("construction")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_construction <- cor_test_mat$construction < 0.02

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_construction]
#factors_construction <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_construction]
data_construction <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_construction$gdp<-NULL #Remove GDP

data_construction[, setdiff(intersect(colnames(data_construction), sectors), 'construction')] <- data_construction %>%
  select(setdiff(intersect(colnames(data_construction), sectors), 'construction')) %>%  as.null

data_construction$transport<-predictions_transport$transport
data_construction$trade<-predictions_trade$trade

# train and test datasets
train <- data_construction %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_construction %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_construction <- dfm(data = train_rolling, p=4, max_iter=100, threshold = 1e-10,
                                 blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_construction) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_construction.xlsx")

predictions_construction<-openxlsx::read.xlsx("predictions_construction.xlsx")


# Real Estate prediction -----------------------------------------------------
target_variable <- "real_estate"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("real_estate")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("real_estate")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_real_estate <- cor_test_mat_t$real_estate < 0.013

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_real_estate]
#factors_real_estate <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_real_estate]
data_real_estate <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_real_estate$gdp<-NULL #Remove GDP

data_real_estate[, setdiff(intersect(colnames(data_real_estate), sectors), 'real_estate')] <- data_real_estate %>%
  select(setdiff(intersect(colnames(data_real_estate), sectors), 'real_estate')) %>%  as.null

#data_real_estate$construction<-predictions_construction$construction

# train and test datasets
train <- data_real_estate %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_real_estate %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_real_estate <- dfm(data = train_rolling, p=2, max_iter=20, threshold = 1e-10,
                                blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_real_estate) %>% 
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

openxlsx::write.xlsx(data_graph_dfm, "predictions_real_estate_dfm.xlsx")

predictions_real_estate<-openxlsx::read.xlsx("predictions_real_estate_dfm.xlsx")


# Finance prediction -----------------------------------------------------
target_variable <- "finance"

# Apply corr.test function
# Print matrix of p-values

cor_test_mat_t <- cor_test_mat[, c("finance")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("finance")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_finance <- cor_test_mat_t$finance < 0.01 #0.01

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_finance]
#factors_finance <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_finance]
data_finance <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_finance$gdp<-NULL #Remove GDP



data_finance[, setdiff(intersect(colnames(data_finance), sectors), 'finance')] <- data_finance %>%
  select(setdiff(intersect(colnames(data_finance), sectors), 'finance')) %>%  as.null


# train and test datasets
train <- data_finance %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_finance %>%
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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_finance <- dfm(data = train_rolling, p=1, max_iter=20, threshold = 1e-10,
                            blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_finance) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_finance_dfm.xlsx")

predictions_finance<-openxlsx::read.xlsx("predictions_finance_dfm.xlsx")


# Others prediction -----------------------------------------------------
target_variable <- "others"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("others")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("others")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_others <- cor_test_mat_t$others < 0.01

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_others]
#factors_others <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_others]
data_others <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_others$gdp<-NULL #Remove GDP

data_others[, setdiff(intersect(colnames(data_others), sectors), 'others')] <- data_others %>%
  select(setdiff(intersect(colnames(data_others), sectors), 'others')) %>%  as.null

#data_others$transport<-predictions_transport$transport
#data_others$trade<-predictions_trade$trade
data_others$mining<-predictions_mining$mining
#data_others$manufacturing<-predictions_manufacturing$manufacturing

data_others$agri<-predictions_agri$agri

# train and test datasets
train <- data_others %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_others %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_others <- dfm(data = train_rolling, p=2, max_iter=20, threshold = 1e-10,
                           blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_others) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_others_dfm.xlsx")

predictions_others<-openxlsx::read.xlsx("predictions_others_dfm.xlsx")


# Taxes prediction -----------------------------------------------------
target_variable <- "taxes"

# Apply corr.test function
#cor_test_mat <- corr.test(data[1:nrow(data),
#                               2:ncol(data)], use = "na.or.complete", method = "pearson")$p
# Print matrix of p-values
cor_test_mat_t <- cor_test_mat[, c("taxes")]

cor_test_mat_t <- as.data.frame(cor_test_mat_t)
colnames(cor_test_mat_t) <- c("taxes")
# cor_test_mat<- rownames_to_column(cor_test_mat)

cor_test_mat_taxes <- cor_test_mat_t$taxes < 0.002375

data_t <- data[2:ncol(data)]

#data2 <- data_t[cor_test_mat_gdp]
#factors_gdp <- colnames(data2)
data2 <- data_t[cor_test_mat_taxes]
#factors_taxes <- intersect(factors_gdp, colnames(data2))
#data2 <- data2[,factors_taxes]
data_taxes <- cbind(data[1], data2)
#data_m <- data_m %>% select(-starts_with('IBC_total'))
#data_m <- data_m %>% select(-starts_with('disp'))
data_taxes$gdp<-NULL #Remove GDP

data_taxes[, setdiff(intersect(colnames(data_taxes), sectors), 'taxes')] <- data_taxes %>%
  select(setdiff(intersect(colnames(data_taxes), sectors), 'taxes')) %>%  as.null


# train and test datasets
train <- data_taxes %>%
  dplyr::filter(date >= train_start_date, date <= train_end_date) %>%
  # the nowcastDFM library only works with dataframe, may get errors in estimation if you use tibbles
  data.frame()
test <- data_taxes %>%
  dplyr::filter(date >= train_start_date, date <= test_end_date) %>% # let test set data begin from training begin, data lag/time series for test sets can go back into the training data
  data.frame()

tail(train)

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
  # training the actual model
  # if (i == 1) { # to save runtime only estimating the model once, not rolling. To estimate rolling, remove this if statement
  train_rolling <- test %>%
    dplyr::filter(date <= seq(as.Date(test_dates[i]), by="-3 months", length=2)[2]) # data as it would have appeared at beginning of prediction period
  output_dfm_taxes <- dfm(data = train_rolling, p=2, max_iter=20, threshold = 1e-10,
                          blocks = blocks)
  print(i)
  #}
  
  # testing the model on artificial vintages  
  for (lag in lags) {
    lagged_data <- gen_lagged_data(metadata, test, test_dates[i], lag) %>% 
      data.frame
    # make sure actual value not in there
    lagged_data[lagged_data$date == test_dates[i], target_variable] <- NA
    prediction <- predict_dfm(lagged_data, output_dfm_taxes) %>% 
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

openxlsx::write.xlsx(dfm_predictions, "predictions_taxes_dfm.xlsx")

predictions_taxes<-openxlsx::read.xlsx("predictions_taxes_dfm.xlsx")

