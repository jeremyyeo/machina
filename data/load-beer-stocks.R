library(quantmod)
library(dplyr)
library(tidyr)

start_date <- "2011-01-03"
end_date   <- Sys.Date() - 1
all_dates  <- data.frame(date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day"))
time_scale <- "daily"

symbols <- c("BUD", "RI.PA", "HEINY")
all_xts <- c()

for (i in symbols) {
  symbol_1 <- i
  data_symbols <- getSymbols(symbol_1, auto.assign = F, periodicity = time_scale)
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
  all_xts <- cbind(all_xts, data_1)
}

all_xts <- data.frame(date = as.Date(index(all_xts)), as.data.frame(all_xts, row.names = F))
