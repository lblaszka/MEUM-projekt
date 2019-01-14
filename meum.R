#!/bin/Rscript
library(TTR)
library(xgboost)
library(TSPred)
library(tseries)
library(forecast)

NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

SCALE_TS_TO_RANGE_1_2 = TRUE

DEBUG = FALSE
USE_MARIMAPRED = FALSE
# Przewiduje 1 do przodu lecz na podstawie nowego modelu.
PREDICT_SINGLE_POINT = FALSE

PLOT_BOXPLOT_ZOOM = FALSE
BOXPLOT_ZOOM = 70

dir.create("output", showWarnings = FALSE)
dir.create("output/ts", showWarnings = FALSE)

# Load NN3 ts
data(NN3.A, NN3.A.cont)

for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
    tmp_ts_learn = as.numeric(unlist(na.omit(NN3.A[i])))
    tmp_ts_test = as.numeric(unlist(na.omit(NN3.A.cont[i])))
    tmp_ts = append(tmp_ts_learn, tmp_ts_test)
    if (SCALE_TS_TO_RANGE_1_2) {
        tmp_ts = tmp_ts - min(tmp_ts)
        tmp_ts = tmp_ts / max(tmp_ts) + 1
    }

    # Create input data features for XGBoost
    month_in_year = 0:(length(tmp_ts)-1) %% 12

    # Relative strength index
    rsi = RSI(tmp_ts, n = 3, maType = "WMA")
    # Create lag in the features to avoid look-ahead bias
    rsi = c(NA, head(rsi, -1))

    input_data = cbind(tmp_ts, month_in_year, rsi)
    input_data = na.omit(input_data)
    colnames(input_data) = c("Data", "Month", "RSI")

    if (DEBUG) {
        print(input_data)
    }

    learn_data_length = dim(input_data)[1] - TEST_DATA_LENGTH
    learn_data = input_data[1:learn_data_length,]
    test_data = input_data[(learn_data_length+1):nrow(input_data),]

    # Split data training and test data into X and Y
    x_learn = learn_data[,2:ncol(input_data)]
    y_learn = learn_data[,1]
    x_test = test_data[,2:ncol(input_data)]
    y_test = test_data[,1]

    xgb_model = xgboost(data=x_learn, label=y_learn, nround=5, objective="reg:linear")
    xgb_forecast = predict(xgb_model, x_test)

    xgb_err = (xgb_forecast - y_test)
    xgb_rel_err = xgb_err / y_test * 100

    if (exists("xgb_relative_error")) {
        xgb_error = cbind(xgb_error, xgb_err)
        xgb_relative_error = cbind(xgb_relative_error, xgb_rel_err)
    } else {
        xgb_error = cbind(xgb_err)
        xgb_relative_error = cbind(xgb_rel_err)
    }

    # XGBoost based on PACF

    # ARIMA
    if (USE_MARIMAPRED) {
        arima_forecast = marimapred( NN3.A[i], NN3.A.cont[i], plot=FALSE )[,1]
    } else {
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
    y_min = min(c(tmp_ts, arima_forecast, xgb_forecast))
    y_max = max(c(tmp_ts, arima_forecast, xgb_forecast))
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
    grid()
    legend(1,
           y_max,
           legend=c("Szereg czasowy", "ARIMA","XGBOOST"),
           col=c("black","blue","red"),
           lty = 1:1)
    dev.off()

    # Plot relative errors
    y_min = min(c(arima_rel_err, xgb_rel_err))
    y_max = max(c(arima_rel_err, xgb_rel_err))
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
    legend(1, y_max, legend=c("ARIMA","XGBOOST"), col=c("blue","red"), lty=1:1)
    dev.off()
}

cat("ARIMA sredni blad, odchylenie standardowe, max, min\n")
print(mean(as.numeric(unlist(arima_error))))
print(sd(as.numeric(unlist(arima_error))))
print(max(abs(as.numeric(unlist(arima_error)))))
print(min(abs(as.numeric(unlist(arima_error)))))
cat("ARIMA sredni blad wzlgedny [%], odchylenie standardowe, max, min\n")
print(mean(as.numeric(unlist(arima_relative_error))))
print(sd(as.numeric(unlist(arima_relative_error))))
print(max(abs(as.numeric(unlist(arima_relative_error)))))
print(min(abs(as.numeric(unlist(arima_relative_error)))))

cat("XGBoost sredni blad, odchylenie standardowe, max, min\n")
print(mean(xgb_error))
print(sd(xgb_error))
print(max(abs(xgb_error)))
print(min(abs(xgb_error)))
cat("XGBoost sredni blad wzlgedny [%], odchylenie standardowe, max, min\n")
print(mean(xgb_relative_error))
print(sd(xgb_relative_error))
print(max(abs(xgb_relative_error)))
print(min(abs(xgb_relative_error)))

# XGBoost relative errors
svg("output/xgb_rel_err_boxplot.svg")
boxplot(as.vector(xgb_relative_error),
        ylab = "Blad wzgledny [%]")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/xgb_rel_err_boxplot_zoom.svg")
    boxplot(as.vector(xgb_relative_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad wzgledny [%]")
    grid()
}

svg("output/xgb_rel_err_boxplot_index.svg")
boxplot(t(xgb_relative_error),
        ylab = "Blad wzgledny [%]")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/xgb_rel_err_boxplot_zoom_index.svg")
    boxplot(t(xgb_relative_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad wzgledny [%]")
    grid()
}

svg("output/xgb_rel_err_density.svg")
plot(density(xgb_relative_error))
grid()

svg("output/xgb_rel_err.svg")
plot(xgb_relative_error[,1],
     ylab = "Blad wzgledny [%]",
     pch = 20,
     ylim=c(round(min(xgb_relative_error))-1, round(max(xgb_relative_error)) +1))
for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
    points(xgb_relative_error[,i],
           pch = 20)
}
grid()

# ARIMA relative errors
arima_relative_error = data.matrix(arima_relative_error)

svg("output/arima_rel_err.svg")
plot(arima_relative_error[,1],
     ylab = "Blad wzgledny [%]",
     pch = 20,
     ylim=c(round(min(arima_relative_error))-1, round(max(arima_relative_error)) +1))
for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
    points(arima_relative_error[,i],
           pch = 20)
}
grid()

svg("output/arima_rel_err_boxplot.svg")
boxplot(as.vector(arima_relative_error),
        ylab = "Blad wzgledny [%]")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/arima_rel_err_boxplot_zoom.svg")
    boxplot(as.vector(arima_relative_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad wzgledny [%]")
    grid()
}

svg("output/arima_rel_err_boxplot_index.svg")
boxplot(t(arima_relative_error),
        ylab = "Blad wzgledny [%]")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/arima_rel_err_boxplot_zoom_index.svg")
    boxplot(t(arima_relative_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad wzgledny [%]")
    grid()
}

svg("output/arima_rel_err_density.svg")
plot(density(arima_relative_error))
grid()

# ARIMA absolute errors
arima_error = data.matrix(arima_error)

svg("output/arima_err.svg")
plot(arima_error[,1],
     ylab = "Blad bezwzgledny",
     pch = 20,
     ylim=c(round(min(arima_error))-1, round(max(arima_error)) +1))
for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
    points(arima_error[,i],
           pch = 20)
}
grid()

svg("output/arima_err_boxplot.svg")
boxplot(as.vector(arima_error),
        ylab = "Blad bezwzgledny")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/arima_err_boxplot_zoom.svg")
    boxplot(as.vector(arima_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad bezwzgledny [%]")
    grid()
}

svg("output/arima_err_boxplot_index.svg")
boxplot(t(arima_error),
        ylab = "Blad bezwzgledny")
grid()

if(PLOT_BOXPLOT_ZOOM) {
    svg("output/arima_err_boxplot_zoom_index.svg")
    boxplot(t(arima_error),
            ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
            ylab = "Blad bezwzgledny")
    grid()
}

svg("output/arima_err_density.svg")
plot(density(arima_error))
grid()
