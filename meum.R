#!/bin/Rscript
library(TTR)
library(xgboost)
library(TSPred)
library(tseries)
library(forecast)

#NUMBER_OF_NN3_TIME_SERIES = 2
NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

DEBUG = FALSE

dir.create("output", showWarnings = FALSE)
dir.create("output/ts", showWarnings = FALSE)

# Load NN3 ts
data(NN3.A, NN3.A.cont)

for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
    tmp_ts = append(as.numeric(unlist(na.omit(NN3.A[i]))),
                    as.numeric(unlist(na.omit(NN3.A.cont[i]))))

    # Create input data features for XGBoost
    month_in_year = 0:(length(tmp_ts)-1) %% 12

    # Relative strength index
    rsi = RSI(tmp_ts, n = 3, maType = "WMA")
    # Potential need to create lag in the features to avoid look-ahead bias?

    input_data = cbind(tmp_ts, month_in_year, rsi)
    colnames(input_data) = c("Data", "Month", "RSI")

    if (DEBUG) {
        print(input_data)
    }

    learn_data_length = length(tmp_ts) - TEST_DATA_LENGTH
    learn_data = input_data[1:learn_data_length,]
    test_data = input_data[(learn_data_length+1):nrow(input_data),]

    # Split data training and test data into X and Y
    x_learn = learn_data[,2:ncol(input_data)]
    y_learn = learn_data[,1]
    x_test = test_data[,2:ncol(input_data)]
    y_test = test_data[,1]

    xgb_model = xgboost(data=x_learn, label=y_learn, nround=5, objective="reg:linear")
    xgb_forecast = predict(xgb_model, x_test)

    xgb_error = (xgb_forecast - y_test) / y_test * 100

    if (exists("xgb_relative_error")) {
        xgb_relative_error = cbind(xgb_relative_error, xgb_error)
    } else {
        xgb_relative_error = cbind(xgb_error)
    }

    # ARIMA
    arima_forecast = marimapred( NN3.A[i], NN3.A.cont[i], plot=FALSE )[,1]
    arima_error = (arima_forecast - NN3.A.cont[i] ) / NN3.A.cont[i] * 100

    if (exists("arima_relative_error")) {
        arima_relative_error = cbind(arima_relative_error, arima_error)
    } else {
        arima_relative_error = cbind(arima_error)
    }

    # Plot forecasts
    y_min = min( c(NN3.A.cont[i][,1], arima_forecast, xgb_forecast ) )
    y_max = max( c(NN3.A.cont[i][,1], arima_forecast, xgb_forecast ) )
    svg(paste('./output/ts/',i,'_forecast.svg', sep=""))
    plot(NULL,
         ylim = c(y_min, y_max ),
         xlim = c(1,18),
         ylab = "Przewidywana wartosc",
         xlab=NULL,
         main=paste('NN3:', i))
    grid()
    lines( NN3.A.cont[i][,1], col="black", lwd= )
    lines( arima_forecast, col="blue" )
    lines( xgb_forecast, col="red" )
    legend(1, y_max, legend=c("cont", "ARIMA","XGBOOST"), col=c("black","blue","red"), lty=1:1)
    dev.off()

    # Plot errors
    y_min = min( c(arima_error[,1], xgb_error ) )
    y_max = max( c(arima_error[,1], xgb_error ) )
    svg( paste('./output/ts/',i,'-ERROR.svg', sep="") )
    plot(NULL,
         ylim = c(y_min, y_max ),
         xlim = c(1,18), ylab = "Blad wzgledny [%]",
         xlab=NULL,
         main=paste('NN3:',i,"Error"))
    grid()
    abline(h=0)
    lines( arima_error[,1], col="blue" )
    lines( xgb_error, col="red" )
    legend(1, y_max, legend=c("ARIMA","XGBOOST"), col=c("blue","red"), lty=1:1)
    dev.off()
}

cat("ARIMA sredni blad wzlgedny [%], odchylenie standardowe, max, min \n")
print(mean(as.numeric(unlist(arima_relative_error))))
print(sd(as.numeric(unlist(arima_relative_error))))
print(max(abs(as.numeric(unlist(arima_relative_error)))))
print(min(abs(as.numeric(unlist(arima_relative_error)))))

cat("XGBoost sredni blad wzlgedny [%], odchylenie standardowe, max, min \n")
print(mean(xgb_relative_error))
print(sd(xgb_relative_error))
print(max(abs(xgb_relative_error)))
print(min(abs(xgb_relative_error)))

# XGBoost
svg("output/xgb_rel_err_boxplot.svg")
boxplot(as.vector(xgb_relative_error),
        ylab = "Blad wzgledny [%]")
grid()
#dev.off()

svg("output/xgb_rel_err_density.svg")
plot(density(xgb_relative_error))
grid()
#dev.off()

svg("output/xgb_errors_common.svg")
plot(xgb_relative_error[,1],
     ylab = "Blad wzgledny [%]",
     pch = 20,
     ylim=c(round(min(xgb_relative_error))-1, round(max(xgb_relative_error)) +1))
for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
    points(xgb_relative_error[,i],
           pch = 20)
}
grid()
#dev.off()

# ARIMA
arima_relative_error = data.matrix( arima_relative_error )
svg("output/arima_rel_err_boxplot.svg")
boxplot(as.vector(arima_relative_error),
        ylab = "Blad wzgledny [%]")
grid()
#dev.off()

svg("output/arima_rel_err_density.svg")
plot(density(arima_relative_error))
grid()
#dev.off()

svg("output/arima_errors_common.svg")
plot(arima_relative_error[,1],
     ylab = "Blad wzgledny [%]",
     pch = 20,
     ylim=c(round(min(arima_relative_error))-1, round(max(arima_relative_error)) +1))
for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
    points(arima_relative_error[,i],
           pch = 20)
}
grid()
#dev.off()
