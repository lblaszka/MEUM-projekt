library(TTR)

NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

DEBUG = TRUE

input_time_series = read.csv('input.csv', header = F)

if (length(input_time_series) != NUMBER_OF_NN3_TIME_SERIES) {
    stop("NN3 benchmark includes ", NUMBER_OF_NN3_TIME_SERIES,
         " time series, found ", length(input_time_series))
}

input_time_series = matrix(unlist(input_time_series, use.names = F),
                           ncol = NUMBER_OF_NN3_TIME_SERIES)

#for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
for (i in 1:1) {
    tmp_ts = na.omit(input_time_series[,i])

    # Create input data features for XGBoost
    month_in_year = 0:length(tmp_ts) %% 12

    # Relative strength index
    rsi = RSI(tmp_ts, n = 3, maType = "WMA")
    # Potential need to create lag in the features to avoid look-ahead bias?

    input_data = cbind(tmp_ts, month_in_year, rsi)
    colnames(input_data) = c("Data", "Month", "RSI")

    if (DEBUG) {
        print(input_data)
    }

    learn_data_length = length(tmp_ts) - TEST_DATA_LENGTH
    learn_data = head(tmp_ts, learn_data_length)
    test_data = tail(tmp_ts, TEST_DATA_LENGTH)
}
