#!/bin/Rscript
library(TTR)
library(xgboost)
library(TSPred)
library(tseries)
library(forecast)

xgb_wrapper <- function(input_data, test_data_len) {
    input_data = na.omit(input_data)

    learn_data_length = dim(input_data)[1] - test_data_len
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

    ret <- list("Forecast" = xgb_forecast,
                "Error" = xgb_err,
                "Relative error" = xgb_rel_err)

    return(ret)
}

print_forecast_stats <- function(error, relative_error, name) {
    cat(paste(name, " sredni blad, odchylenie standardowe, max, min\n"))
    print(mean(as.numeric(unlist(error))))
    print(sd(as.numeric(unlist(error))))
    print(max(abs(as.numeric(unlist(error)))))
    print(min(abs(as.numeric(unlist(error)))))
    cat(paste(name, " sredni blad wzlgedny [%], odchylenie standardowe, max, min\n"))
    print(mean(as.numeric(unlist(relative_error))))
    print(sd(as.numeric(unlist(relative_error))))
    print(max(abs(as.numeric(unlist(relative_error)))))
    print(min(abs(as.numeric(unlist(relative_error)))))
}

plot_forecast_errors <- function(error, relative_error, name) {
    relative_error = data.matrix(relative_error)
    
    svg(paste('output/', name, '_rel_err.svg'))
    plot(relative_error[,1],
         ylab = "Blad wzgledny [%]",
         pch = 20,
         ylim=c(round(min(relative_error))-1, round(max(relative_error)) +1),
         main = name)
    for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
        points(relative_error[,i],
               pch = 20)
    }
    grid()
    
    svg(paste('output/', name, '_rel_err_boxplot.svg'))
    boxplot(as.vector(relative_error), ylab = "Blad wzgledny [%]", main = name)
    grid()
    
    if(PLOT_BOXPLOT_ZOOM) {
        svg(paste('output/', name, '_rel_err_boxplot_zoom.svg'))
        boxplot(as.vector(relative_error),
                ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
                ylab = "Blad wzgledny [%]",
                main = name)
        grid()
    }
    
    svg(paste('output/', name, '_rel_err_boxplot_index.svg'))
    boxplot(t(relative_error), ylab = "Blad wzgledny [%]", main = name)
    grid()
    
    if (PLOT_BOXPLOT_ZOOM) {
        svg(paste('output/', name, '_rel_err_boxplot_zoom_index.svg'))
        boxplot(t(relative_error),
                ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
                ylab = "Blad wzgledny [%]",
                main = name)
        grid()
    }
    
    svg(paste('output/', name, '_rel_err_density.svg'))
    plot(density(relative_error), main = name)
    grid()
    
    # ARIMA absolute errors
    error = data.matrix(error)
    
    svg(paste('output/', name, '_err.svg'))
    plot(error[,1],
         ylab = "Blad bezwzgledny",
         pch = 20,
         ylim=c(round(min(error))-1, round(max(error)) +1),
         main = name)
    for (i in 2:NUMBER_OF_NN3_TIME_SERIES) {
        points(error[,i],
               pch = 20)
    }
    grid()
    
    svg(paste('output/', name, '_err_boxplot.svg'))
    boxplot(as.vector(error),
            ylab = "Blad bezwzgledny",
            main = name)
    grid()
    
    if(PLOT_BOXPLOT_ZOOM) {
        svg(paste('output/', name, '_err_boxplot_zoom.svg'))
        boxplot(as.vector(error),
                ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
                ylab = "Blad bezwzgledny [%]",
                main = name)
        grid()
    }
    
    svg(paste('output/', name, '_err_boxplot_index.svg'))
    boxplot(t(error),
            ylab = "Blad bezwzgledny",
            main = name)
    grid()
    
    if(PLOT_BOXPLOT_ZOOM) {
        svg('output/', name, '_err_boxplot_zoom_index.svg')
        boxplot(t(error),
                ylim = c(-BOXPLOT_ZOOM, BOXPLOT_ZOOM),
                ylab = "Blad bezwzgledny",
                main = name)
        grid()
    }
    
    svg(paste('output/', name, '_err_density.svg'))
    plot(density(error), main = name)
    grid()
}
