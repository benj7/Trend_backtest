
asset <- "axa"
daily <- readRDS(str_c("output/data/", asset, "_daily_modified.rds"))
weekly <-  readRDS(str_c("output/data/", asset, "_weekly_modified.rds"))
monthly <-  readRDS(str_c("output/data/", asset, "_monthly_modified.rds"))

# Focus sur une stratégie Relance de puissance + Trend following 
sdcols <- c("date", "last_day_week", "last_day_month", "week_year",
            "adj_close", "bol_inf", "sma20", "bol_sup", "distance_bol_sup",
            "sma20_weekly", "distance_bol_sup_weekly", "sma20_monthly",
            "distance_bol_sup_monthly", "safeline", "safelinelow",
            "ok_relance")

daily_strat <- daily[1:20, .SD, .SDcols = sdcols]


max_dates <- daily_strat[, .(max_date = max(date)), by = .(week(date), year(date))][, max_date]

dt <- dt[date %in% max_dates, last_day_week := 1]
