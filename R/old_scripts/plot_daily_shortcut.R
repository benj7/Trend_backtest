start_date <- "2003-07-01"
end_date <- "2003-12-30"

plot_daily <- plot_candlestick(data_daily,
                               start_date = start_date,
                               end_date = end_date,
                               name = "CAC40")
plot_daily
