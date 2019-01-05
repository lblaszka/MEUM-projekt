library(TTR)
library(xgboost)
library(TSPred)

NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

DEBUG = TRUE

# Load train data
data(NN3.A)
# Load test data
data(NN3.A.cont)

#for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
for (i in 1:1) {
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

    xgb_error = (xgb_forecast - y_test) / y_test

    if (exists("xgb_relative_error")) {
        xgb_relative_error = cbind(xgb_relative_error, xgb_error)
    } else {
        xgb_relative_error = cbind(xgb_error)
    }
}
