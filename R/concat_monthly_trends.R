
monthly_trends_files <- list.files(path = "output/data/",
                                   pattern = "trend_mensuel")

monthly_trends <- map_dfr(as.list(str_c("output/data/", monthly_trends_files)), 
                          readRDS)

monthly_trends <- as.data.table(monthly_trends)

monthly_trends <- monthly_trends[order(date_analyse)]

date_min <- monthly_trends[, .(min_date = min(date_analyse)), by = .(asset)][, max(min_date)]

monthly_trends <- monthly_trends[date_analyse >= date_min]

tmp <- daily[last_day_month == 1,
             .(date, adj_close, sma20, sma20_weekly, sma20_monthly,
               distance_bol_sup, distance_bol_sup_weekly, distance_bol_sup_monthly)]

tmp <- tmp[distance_bol_sup_monthly >= -0.5  & adj_close > sma20_weekly & adj_close > sma20]
