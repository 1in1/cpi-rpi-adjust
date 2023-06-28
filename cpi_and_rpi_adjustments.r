library("tidyverse")

# Base values we are interested in
base.date <- "2021 JUL"
base.val <- 100

df.cpi <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23")
df.rpi <- read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23")

adjusted_time_series <- function(index_time_series, init_date, init_val) {
  df.stripped <- filter(
    index_time_series,
    str_detect(Title, "\\d{4} [A-Z]{3}")
  )
  names(df.stripped) <- c("Month", "Value")
  
  df.stripped$Date <- ym(df.stripped$Month) + 14 # These datasets are taken around the 15th
  init_date_midmonth <- ym(init_date) + 14
  
  base <- as.numeric(df.stripped[df.stripped$Month == init_date, 2])
  df.stripped <- transform(
    filter(df.stripped, Date >= init_date_midmonth), 
    Value = init_val * as.numeric(Value) / base
  )
}

adj.cpi <- adjusted_time_series(df.cpi, base.date, base.val)
adj.rpi <- adjusted_time_series(df.rpi, base_date, base.val)
plot(
  Value ~ Date,
  adj.rpi,
  xlab = "Date",
  ylab = "Value",
  xaxt = "n", 
  col='red', 
  type = "l"
)
lines(
  Value ~ Date,
  adj.cpi,
  xaxt = "n", 
  col='blue', 
  type = "l"
)
axis(
  1, 
  adj.cpi$Date, 
  format(adj.cpi$Date, "%b %d"), 
  cex.axis = .8
)
