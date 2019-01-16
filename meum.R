#!/bin/Rscript
library(TTR)
library(xgboost)
library(TSPred)
library(tseries)
library(forecast)
library(data.table)
library(robustHD)
library(tictoc)
source('meum_functions.R')

NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

SCALE_TS_TO_RANGE_1_2 = TRUE
STANDARIZE_TS = FALSE

DEBUG = FALSE
# Przewiduje 1 do przodu lecz na podstawie nowego modelu.
PREDICT_SINGLE_POINT = FALSE

PLOT_BOXPLOT_ZOOM = FALSE
BOXPLOT_ZOOM = 70

PACF_LAG_THRESHOLD = 0.1

dir.create("output", showWarnings = FALSE)
dir.create("output/ts", showWarnings = FALSE)

# Load NN3 ts
data(NN3.A, NN3.A.cont)

tic()
for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
    cat(paste("Iteracja: ",i,"\n"))

    tmp_ts_learn = as.numeric(unlist(na.omit(NN3.A[i])))
    tmp_ts_test = as.numeric(unlist(na.omit(NN3.A.cont[i])))
    tmp_ts = append(tmp_ts_learn, tmp_ts_test)
    if (SCALE_TS_TO_RANGE_1_2) {
        tmp_ts = tmp_ts - min(tmp_ts)
        tmp_ts = tmp_ts / max(tmp_ts) + 1
    } else if (STANDARIZE_TS) {
        tmp_ts = standardize(tmp_ts)
    }

    # Create input data features for XGBoost
    month_in_year = 0:(length(tmp_ts)-1) %% 12

    # Relative strength index
    rsi = RSI(tmp_ts, n = 3, maType = "WMA")
    # Create lag in the features to avoid look-ahead bias
    rsi = c(NA, head(rsi, -1))

    xgb_input_data = cbind(tmp_ts, month_in_year, rsi)
    colnames(xgb_input_data) = c("Data", "Month", "RSI")

    if (DEBUG) {
        print(xgb_input_data)
    }

    xgb_res = xgb_wrapper(xgb_input_data, TEST_DATA_LENGTH)
    xgb_forecast = xgb_res$'Forecast'
    xgb_err = xgb_res$'Error'
    xgb_rel_err = xgb_res$'Relative error'

    if (exists("xgb_relative_error")) {
        xgb_error = cbind(xgb_error, xgb_err)
        xgb_relative_error = cbind(xgb_relative_error, xgb_rel_err)
    } else {
        xgb_error = cbind(xgb_err)
        xgb_relative_error = cbind(xgb_rel_err)
    }

    # XGBoost based on PACF
    xgb_pacf_input_data = cbind(tmp_ts)

    tmp_pacf = pacf(tmp_ts_learn, lag = 12)
    tmp_pacf = tmp_pacf$acf
    for (pacf_i in 1:12) {
        if (abs(tmp_pacf[pacf_i]) >= PACF_LAG_THRESHOLD) {
            tmp_ts_lagged = shift(tmp_ts, pacf_i)
            xgb_pacf_input_data = cbind(xgb_pacf_input_data, tmp_ts_lagged)
        }
    }

    xgb_pacf_res = xgb_wrapper(xgb_pacf_input_data, TEST_DATA_LENGTH)
    xgb_pacf_forecast = xgb_pacf_res$'Forecast'
    xgb_pacf_err = xgb_pacf_res$'Error'
    xgb_pacf_rel_err = xgb_pacf_res$'Relative error'

    if (exists("xgb_pacf_relative_error")) {
        xgb_pacf_error = cbind(xgb_pacf_error, xgb_pacf_err)
        xgb_pacf_relative_error = cbind(xgb_pacf_relative_error, xgb_pacf_rel_err)
    } else {
        xgb_pacf_error = cbind(xgb_pacf_err)
        xgb_pacf_relative_error = cbind(xgb_pacf_rel_err)
    }

    # ARIMA
    if (TRUE) {
        if(PREDICT_SINGLE_POINT) {
            arima_forecast_oneFront = 1:TEST_DATA_LENGTH
            for (j in 1:TEST_DATA_LENGTH) {
                ts_end = learn_data_length + j
                ts <- ts(tmp_ts[1:ts_end])
                nobs <- length(ts)
                reg <- cbind(1:nobs)
                xreg = ts(reg,start=1)
                newreg <- ts_end + 1
                arima_model = auto.arima(ts, xreg = xreg)
                arima_forecast_all = forecast(arima_model, h = 1, xreg = newreg)
                arima_forecast = as.data.frame(arima_forecast_all)$'Point Forecast'
                arima_forecast_oneFront[j] = arima_forecast
            }
            arima_forecast = arima_forecast_oneFront
        } else {
            nobs <- length(tmp_ts)
            xreg <- cbind(1:nobs)
            newreg <- (nobs+1):(nobs+TEST_DATA_LENGTH)
            arima_model = auto.arima(tmp_ts, xreg = xreg)
            arima_forecast_all = forecast(arima_model, h = TEST_DATA_LENGTH, xreg = newreg)
            arima_forecast = as.data.frame(arima_forecast_all)$'Point Forecast'
        }
    }

    arima_err = (arima_forecast - tail(tmp_ts, TEST_DATA_LENGTH))
    arima_rel_err = arima_err / tail(tmp_ts, TEST_DATA_LENGTH) * 100

    if (exists("arima_relative_error")) {
        arima_error = cbind(arima_error, arima_err)
        arima_relative_error = cbind(arima_relative_error, arima_rel_err)
    } else {
        arima_error = cbind(arima_err)
        arima_relative_error = cbind(arima_rel_err)
    }

    # Plot time series with forecasts
    y_min = min(c(tmp_ts, arima_forecast, xgb_forecast, xgb_pacf_forecast))
    y_max = max(c(tmp_ts, arima_forecast, xgb_forecast, xgb_pacf_forecast))
    svg(paste('./output/ts/',i,'.svg', sep=""))
    plot(NULL,
         xlim = c(1, length(tmp_ts)),
         ylim = c(y_min, y_max),
         ylab = "Wartość",
         main = paste('NN3 TS:', i))
    lines(tmp_ts, col = "black")
    tmp_x = 1:TEST_DATA_LENGTH
    tmp_x = tmp_x + length(tmp_ts_learn)
    lines(x = tmp_x, y = arima_forecast, col = "blue")
    lines(x = tmp_x, y = xgb_forecast, col = "red")
    lines(x = tmp_x, y = xgb_pacf_forecast, col = "green")
    grid()
    legend(1,
           y_max,
           legend=c("Szereg czasowy", "ARIMA","XGBoost", "XGBoost PACF"),
           col=c("black","blue","red"),
           lty = 1:1)
    dev.off()

    # Plot absolute errors
    y_min = min(c(arima_err, xgb_err, xgb_pacf_err))
    y_max = max(c(arima_err, xgb_err, xgb_pacf_err))
    svg(paste('./output/ts/',i,'-ERROR.svg', sep=""))
    plot(NULL,
         ylim = c(y_min, y_max),
         ylab = "Wartość",
         xlim = c(1,18),
         xlab = NULL,
         main = paste('NN3:',i,"Błąd bezwzględny"))
    grid()
    abline(h=0)
    lines(arima_err, col = "blue")
    lines(xgb_err, col = "red")
    lines(xgb_pacf_err, col = "green")
    legend(1,y_max, legend=c("ARIMA","XGBoost","XGBoost PACF"), col=c("blue","red","green"), lty=1:1)
    dev.off()

    # Plot relative errors
    y_min = min(c(arima_rel_err, xgb_rel_err, xgb_pacf_rel_err))
    y_max = max(c(arima_rel_err, xgb_rel_err, xgb_pacf_rel_err))
    svg(paste('./output/ts/',i,'-RELATIVE_ERROR.svg', sep=""))
    plot(NULL,
         ylim = c(y_min, y_max),
         ylab = "Wartość [%]",
         xlim = c(1,18),
         xlab = NULL,
         main = paste('NN3:',i,"Błąd względny"))
    grid()
    abline(h=0)
    lines(arima_rel_err, col = "blue")
    lines(xgb_rel_err, col = "red")
    lines(xgb_pacf_rel_err, col = "green")
    legend(1,y_max, legend=c("ARIMA","XGBoost","XGBoost PACF"), col=c("blue","red","green"), lty=1:1)
    dev.off()
}

print_forecast_stats(arima_error, arima_relative_error, 'ARIMA')
print_forecast_stats(xgb_error, xgb_relative_error, 'XGBoost')
print_forecast_stats(xgb_pacf_error, xgb_pacf_relative_error, 'XGBoost PACF')

plot_forecast_errors(arima_error, arima_relative_error, 'ARIMA')
plot_forecast_errors(xgb_error, xgb_relative_error, 'XGB')
plot_forecast_errors(xgb_pacf_error, xgb_pacf_relative_error, 'XGB_PACF')

# Plot densites on common figure
d_arima_error = density(arima_error)
d_xgb_error = density(xgb_error)
d_xgb_pacf_error = density(xgb_pacf_error)
y_min = min(d_arima_error$y, d_xgb_error$y, d_xgb_pacf_error$y)
y_max = max(d_arima_error$y, d_xgb_error$y, d_xgb_pacf_error$y)
x_min = min(d_arima_error$x, d_xgb_error$x, d_xgb_pacf_error$x)
x_max = max(d_arima_error$x, d_xgb_error$x, d_xgb_pacf_error$x)
svg(paste('output/err_density_common.svg'))
plot(d_arima_error,
     col = 'blue',
     xlim = c(x_min, x_max),
     ylim = c(y_min, y_max),
     main = 'Gęstość błędów bezwzglednych')
lines(d_xgb_error, col = 'red')
lines(d_xgb_pacf_error, col = 'green')
legend(x_min, y_max, legend = c("ARIMA","XGBoost","XGBoost PACF"), col=c("blue","red","green"), lty=1:1)
grid()

d_arima_rel_error = density(arima_relative_error)
d_xgb_rel_error = density(xgb_relative_error)
d_xgb_pacf_rel_error = density(xgb_pacf_relative_error)
y_min = min(d_arima_rel_error$y, d_xgb_rel_error$y, d_xgb_pacf_rel_error$y)
y_max = max(d_arima_rel_error$y, d_xgb_rel_error$y, d_xgb_pacf_rel_error$y)
x_min = min(d_arima_rel_error$x, d_xgb_rel_error$x, d_xgb_pacf_rel_error$x)
x_max = max(d_arima_rel_error$x, d_xgb_rel_error$x, d_xgb_pacf_rel_error$x)
svg(paste('output/rel_err_density_common.svg'))
plot(d_arima_rel_error,
     col = 'blue',
     xlim = c(x_min, x_max),
     ylim = c(y_min, y_max),
     main = 'Gęstość błędów wzglednych')
lines(d_xgb_rel_error, col = 'red')
lines(d_xgb_pacf_rel_error, col = 'green')
legend(x_min, y_max, legend = c("ARIMA","XGBoost","XGBoost PACF"), col=c("blue","red","green"), lty=1:1)
grid()

toc()
