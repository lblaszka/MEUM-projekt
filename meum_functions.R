#!/bin/Rscript
library(TTR)
library(xgboost)
library(TSPred)
library(tseries)
library(forecast)

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

