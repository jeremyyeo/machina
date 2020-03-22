# For dev/testing.

# Test forecasting.
library(prophet)

my_data <- read.csv("data.csv")
m <- prophet(my_data)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

plot(m, forecast)

x <- prophet_plot_components(m, forecast)
grid.arrange(grobs = x)

# CausalImpact test ----
library(quantmod)
library(dygraphs)
library(dplyr)
library(tidyr)
library(CausalImpact)

start_date <- "2011-01-03"
end_date   <- Sys.Date() - 1
all_dates  <- data.frame(date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day"))
time_scale <- "daily"

symbol_1 <- "BUD"
data_symbols <- getSymbols(symbol_1, auto.assign = F, periodicity = time_scale)
# data_symbols <- na.fill(data_symbols, "extend")
data_symbols <- data.frame(date = index(data_symbols), coredata(data_symbols)) %>% filter(date >= start_date)
colnames(data_symbols) <- c("date", "open", "high", "low", "close", "volume", "adjusted_close")

data_symbols_xts <-
  data_symbols %>%
  dplyr::select(date, adjusted_close) %>%
  full_join(all_dates) %>%
  arrange(date) %>%
  fill(adjusted_close)

rownames(data_symbols_xts) <- data_symbols_xts[[1]]
data_1 <- data_symbols_xts %>% dplyr::select(adjusted_close) %>% dplyr::rename(!!symbol_1 := adjusted_close) %>% as.xts()

symbol_2 <- "RI.PA"
data_symbols <- getSymbols(symbol_2, auto.assign = F, periodicity = time_scale)
# data_symbols <- na.fill(data_symbols, "extend")
data_symbols <- data.frame(date = index(data_symbols), coredata(data_symbols)) %>% filter(date >= start_date)
colnames(data_symbols) <- c("date", "open", "high", "low", "close", "volume", "adjusted_close")

data_symbols_xts <-
  data_symbols %>%
  dplyr::select(date, adjusted_close) %>%
  full_join(all_dates) %>%
  arrange(date) %>%
  fill(adjusted_close)
rownames(data_symbols_xts) <- data_symbols_xts[[1]]
data_2 <- data_symbols_xts %>% dplyr::select(adjusted_close) %>% dplyr::rename(!!symbol_2 := adjusted_close) %>% as.xts()

symbol_3 <- "HEINY"
data_symbols <- getSymbols(symbol_3, auto.assign = F, periodicity = time_scale)
# data_symbols <- na.fill(data_symbols, "extend")
data_symbols <- data.frame(date = index(data_symbols), coredata(data_symbols)) %>% filter(date >= start_date)
colnames(data_symbols) <- c("date", "open", "high", "low", "close", "volume", "adjusted_close")

data_symbols_xts <-
  data_symbols %>%
  dplyr::select(date, adjusted_close) %>%
  full_join(all_dates) %>%
  arrange(date) %>%
  fill(adjusted_close)
rownames(data_symbols_xts) <- data_symbols_xts[[1]]
data_3 <- data_symbols_xts %>% dplyr::select(adjusted_close) %>% dplyr::rename(!!symbol_3 := adjusted_close) %>% as.xts()

all_xts <- cbind(data_1, data_2, data_3)
all_zoo <- zoo(all_xts, all_dates$date)
write.zoo(all_zoo, file = "beer_stocks.csv", index.name = "date", sep = ",")
dygraph(all_zoo) %>% dyShading(from = "2020-01-01", to = end_date) %>% dyRangeSelector()

pre_period  <- as.Date(c("2011-01-03", "2020-01-01"))
post_period <- as.Date(c("2020-01-02", toString(end_date)))
impact      <- CausalImpact(all_zoo, pre_period, post_period)
plot(impact)
print_out <- summary(impact, "report")

dygraph(impact$series[, c("response", "point.pred", "point.pred.lower", "point.pred.upper")]) %>%
  dySeries("response", label = "Actual", color = "#5E81AC") %>%
  dySeries(c("point.pred.lower", "point.pred", "point.pred.upper"), label = "Prediction", color = "#BF616A") %>%
  dyShading(from = "2020-01-01", to = end_date) %>%
  dyRangeSelector()

