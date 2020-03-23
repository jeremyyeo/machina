library(prophet)
p_data   <- read.csv("usr/local/src/data/prophet-demo.csv")
model    <- prophet(p_data)
future   <- make_future_dataframe(model, periods = 365)
forecast <- predict(model, future)
if (exists("forecast")) {
  print("prophet-prep success")
}
