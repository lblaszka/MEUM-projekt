NUMBER_OF_NN3_TIME_SERIES = 111
TEST_DATA_LENGTH = 18

input_time_series = read.csv('input.csv', header = F)

if (length(input_time_series) != NUMBER_OF_NN3_TIME_SERIES) {
    stop("NN3 benchmark includes ", NUMBER_OF_NN3_TIME_SERIES,
         " time series, found ", length(input_time_series))
}

input_time_series = matrix(unlist(input_time_series, use.names = F),
                           ncol = NUMBER_OF_NN3_TIME_SERIES)

for (i in 1:NUMBER_OF_NN3_TIME_SERIES) {
    tmp_time_series = na.omit(input_time_series[,i])
    test_data = tail(tmp_time_series, TEST_DATA_LENGTH)
}
