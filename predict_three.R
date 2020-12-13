library(data.table)
library(dplyr)
library(forecast)
library(gbm)
library(glue)
library(neuralnet)
library(randomForest)
library(DT)
library(Metrics)
library(zoo)
library(prophet)
library(xgboost)

# This is required to produce same random numbers during every run
set.seed(4223842)

##
## Helper Functions
##
calculate.smape <- function(frc, validate) {
  mean(200 * abs(validate - as.numeric(frc)) / (abs(validate) + abs(as.numeric(frc))))
}

calculate.rmse <- function(frc, validate) {
  rmse(validate, as.numeric(frc))
}

replace_missing_weekend_prices <- function(df) {
  return(na.locf(df))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

##
## CSVs location
##
csvDirectory       <- "C:/Users/alexandra/Desktop/ΔΙΠΛΩΜΑΤΙΚΗ/alexandra/rstudio"

##
# Parameters
##
dataFrequency      <- 365
predictionPeriod   <- 3

##
## Dataset split to train, validate, test
##
df                 <- read.csv(paste(csvDirectory, "all.csv", sep="/"), stringsAsFactors=F)
df                 <- replace_missing_weekend_prices(df)

# Split to Train, Validate, Test
trainDf            <- head(df, nrow(df) - 2 * as.integer(nrow(df) * 0.1))
valtestDf          <- tail(df, nrow(df) - nrow(trainDf))
validateDf         <- head(df, nrow(valtestDf) / 2)
testDf             <- tail(df, nrow(valtestDf) / 2)

##
## Evaluation Matrix
##
Evaluation            <- data.frame(matrix(NA, ncol=1, nrow=7))
row.names(Evaluation) <- c(
  "Naive", "HOLT", "Random Forest", "GBM",
  "ARIMA", "Prophet", "XGBoost")
colnames(Evaluation)  <- c("RMSE")

##
## Fine-tune Hyperparameters
##

# Hyperparameter Grids
hyper_grid_hw <- expand.grid(
  alpha = seq(0.5, 0.9, by=0.1),
  beta  = seq(0.1, 0.4, by=0.1)
)

hyper_grid_rf <- expand.grid(
  ntree      = c(500, 1000),
  mtry       = c(2, 3, 4)
)

hyper_grid_gbm <- expand.grid(
  n.trees           = c(500, 1000, 2000),
  interaction.depth = c(2, 3, 4),
  shrinkage         = c(0.1, 0.01, 0.001)
)

hyper_grid_arima <- expand.grid(
  p = c(1, 2, 3),
  d = c(1, 2, 3),
  q = c(1, 2, 3)
)

hyper_grid_xgboost <- expand.grid(
  eta       = c(0.2, 0.1, 0.05),
  max_depth = c(2, 4, 6, 10)
)

# Initialize empty data frames and vectors to hold calculated RMSEs
rmses.naive   <- c()
rmses.hw      <- data.frame(matrix(nrow=nrow(hyper_grid_hw), ncol = 0))
rmses.rf      <- data.frame(matrix(nrow=nrow(hyper_grid_rf), ncol = 0))
rmses.gbm     <- data.frame(matrix(nrow=nrow(hyper_grid_gbm), ncol = 0))
rmses.arima   <- data.frame(matrix(nrow=nrow(hyper_grid_arima), ncol = 0))
rmses.prophet <- c()
rmses.xgboost <- data.frame(matrix(nrow=nrow(hyper_grid_xgboost), ncol = 0))

# For-loop for validation over validateDf for every predictionPeriod-length subsample
# Keep track of time
start_time <- Sys.time()
for (i in 1:(nrow(validateDf) %/% predictionPeriod)) {
  temp_trainDf    <- bind_rows(trainDf, validateDf[1:(predictionPeriod * i), ])
  temp_validateDf <- head(
    validateDf[(predictionPeriod * i + 1):nrow(validateDf), ],
    predictionPeriod)
  temp_trainDfTs  <- ts(temp_trainDf$Bitcoin, frequency=dataFrequency)
  
  # Naive
  frc1              <- naive(temp_trainDfTs, h=nrow(temp_validateDf))$mean
  rmses.naive       <- c(rmses.naive, calculate.rmse(frc1, temp_validateDf$Bitcoin))

  # Holt-Winters
  rmses.hw.temp <- c()
  for (j in 1:nrow(hyper_grid_hw)) {
    hw   <- HoltWinters(temp_trainDfTs, gamma=FALSE,
                        alpha=hyper_grid_hw$alpha[j],
                        beta=hyper_grid_hw$beta[j]
    )
    frc2 <- forecast(hw, h=nrow(temp_validateDf))$mean
    rmses.hw.temp <- c(rmses.hw.temp, calculate.rmse(frc2, temp_validateDf$Bitcoin))
  }
  rmses.hw$temp <- rmses.hw.temp
  names(rmses.hw)[names(rmses.hw) == "temp"] <- i
  
  # Random Forest - RF
  rmses.rf.temp <- c()
  for (j in 1:nrow(hyper_grid_rf)) {
    rf_model  <- randomForest(Bitcoin ~ Litecoin + Monero + Dash + Ethereum +
                                Oil + Copper + Bitcoin.Hashrate, data=temp_trainDf,
                              mtry=hyper_grid_rf$mtry[j],
                              ntree=hyper_grid_rf$ntree[j]
    )

    frc3      <- predict(rf_model, temp_validateDf)
    rmses.rf.temp <- c(rmses.rf.temp, calculate.rmse(frc3, temp_validateDf$Bitcoin))
  }
  rmses.rf$temp <- rmses.rf.temp
  names(rmses.rf)[names(rmses.rf) == "temp"] <- i

  # Gradient Boosting Machine - GBM
  rmses.gbm.temp <- c()
  for (j in 1:nrow(hyper_grid_gbm)) {
    gbm_model <- gbm(Bitcoin ~ Litecoin + Monero + Dash + Ethereum +
                       Oil + Copper + Bitcoin.Hashrate, data=temp_trainDf,
                     distribution="gaussian",
                     n.trees = hyper_grid_gbm$n.trees[j],
                     interaction.depth = hyper_grid_gbm$interaction.depth[j],
                     shrinkage = hyper_grid_gbm$shrinkage[j]
    )

    frc4           <- predict(gbm_model, temp_validateDf, n.trees=gbm_model$n.trees)
    rmses.gbm.temp <- c(rmses.gbm.temp, calculate.rmse(frc4, temp_validateDf$Bitcoin))
  }
  rmses.gbm$temp <- rmses.gbm.temp
  names(rmses.gbm)[names(rmses.gbm) == "temp"] <- i

  # ARIMA
  rmses.arima.temp <- c()
  for (j in 1:nrow(hyper_grid_arima)) {
    arima_model       <- arima(temp_trainDfTs,
                               c(hyper_grid_arima$p[j],
                                 hyper_grid_arima$d[j],
                                 hyper_grid_arima$q[j])
    )
    frc5             <- predict(arima_model, nrow(temp_validateDf))$pred
    rmses.arima.temp <- c(rmses.arima.temp, calculate.rmse(frc5, temp_validateDf$Bitcoin))
  }
  rmses.arima$temp <- rmses.arima.temp
  names(rmses.arima)[names(rmses.arima) == "temp"] <- i

  # Prophet
  prophetTrainDf       <- temp_trainDf
  prophetTrainDf$ds    <- prophetTrainDf$Date
  prophetTrainDf$y     <- prophetTrainDf$Bitcoin
  prophet_model        <- prophet(prophetTrainDf,
                                  weekly.seasonality=FALSE, daily.seasonality=FALSE,
                                  seasonality.mode="multiplicative")
  prophetValidateDf    <- temp_validateDf
  prophetValidateDf$ds <- prophetValidateDf$Date
  prophetValidateDf$y  <- prophetValidateDf$Bitcoin
  frc6                 <- predict(prophet_model, prophetValidateDf)
  rmses.prophet        <- c(rmses.prophet, calculate.rmse(frc6$yhat, prophetValidateDf$y))

  # XGBoost
  rmses.xgboost.temp   <- c()
  temp_trainDf_data    <- as.matrix(subset(temp_trainDf, select=c(Litecoin, Monero, Dash, Ethereum, Oil, Copper, Bitcoin.Hashrate)))
  temp_trainDf_label   <- as.matrix(subset(temp_trainDf, select=c(Bitcoin)))
  temp_validateDf_data <- as.matrix(subset(temp_validateDf, select=c(Litecoin, Monero, Dash, Ethereum, Oil, Copper, Bitcoin.Hashrate)))
  for (j in 1:nrow(hyper_grid_xgboost)) {
    bst_model <- xgboost(data=temp_trainDf_data,
                         label=temp_trainDf_label,
                         nrounds=50, nthread=2, objective="reg:squarederror", eval_metric="rmse", verbose=0,
                         max_depth=hyper_grid_xgboost$max_depth[j],
                         eta=hyper_grid_xgboost$eta[j]
    )
    frc7               <- predict(bst_model, temp_validateDf_data)
    rmses.xgboost.temp <- c(rmses.xgboost.temp, calculate.rmse(frc7, temp_validateDf$Bitcoin))
  }
  rmses.xgboost$temp <- rmses.xgboost.temp
  names(rmses.xgboost)[names(rmses.xgboost) == "temp"] <- i
}
Evaluation$RMSE[1] <- mean(rmses.naive)
Evaluation$RMSE[2] <- min(rowMeans(rmses.hw))
Evaluation$RMSE[3] <- min(rowMeans(rmses.rf))
Evaluation$RMSE[4] <- min(rowMeans(rmses.gbm))
Evaluation$RMSE[5] <- min(rowMeans(rmses.arima))
Evaluation$RMSE[6] <- mean(rmses.prophet)
Evaluation$RMSE[7] <- min(rowMeans(rmses.xgboost))

print("Best Holt-Winters")
print(hyper_grid_hw[which.min(rowMeans(rmses.hw)), ])
print("Best Random Forest")
print(hyper_grid_rf[which.min(rowMeans(rmses.rf)), ])
print("Best GBM")
print(hyper_grid_gbm[which.min(rowMeans(rmses.gbm)), ])
print("Best ARIMA")
print(hyper_grid_arima[which.min(rowMeans(rmses.arima)), ])
print("Best XGBoost")
print(hyper_grid_xgboost[which.min(rowMeans(rmses.xgboost)), ])

# Keep track of time
time_lapsed <- Sys.time() - start_time
print(glue("Time lapsed: {time_lapsed}"))