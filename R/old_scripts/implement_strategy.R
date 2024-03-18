

data_daily <- as.data.table(data_daily)
data_weekly <- as.data.table(data_weekly)
data_monthly <- as.data.table(data_monthly)
data_daily <- create_key_joins(data_daily)
data_weekly<- create_key_joins(data_weekly)
data_monthly<- create_key_joins(data_monthly)

data_daily <- order_date(data_daily)
data_weekly <- order_date(data_weekly)
data_monthly <- order_date(data_monthly)

data_daily <- add_bollinger(data_daily,
                            n = 20,
                            sd = 0.75)

data_weekly <- add_bollinger(data_weekly,
                             n = 20,
                             sd = 0.75)

data_monthly <- add_bollinger(data_monthly,
                              n = 20,
                              sd = 0.5)


data_daily <- add_moving_averages(data_daily)
data_weekly <- add_moving_averages(data_weekly)
data_monthly <- add_moving_averages(data_monthly)

# CAC 40 MONTREUX 


data_daily <- above_bollinger(data_daily)
data_weekly <- above_bollinger(data_weekly)
data_monthly <- above_bollinger(data_monthly)



data_daily <- below_bollinger(data_daily)
data_weekly <- below_bollinger(data_weekly)
data_monthly <- below_bollinger(data_monthly)

tmp <- data_daily[above_bol_sup == 1]

# Trouver périodes où les 3 timeframes sont au dessus des boll 

dates_monthly_ok <- data_monthly[above_bol_sup == 1,
                                 .(Date, month_year)]

uniques_dates_monthly_ok <- unique(dates_monthly_ok$month_year)

dates_weekly_ok <- data_weekly[above_bol_sup == 1,
                               .(Date, week_year, month_year)]

dates_weekly_ok <- dates_weekly_ok[month_year %in% uniques_dates_monthly_ok]

uniques_dates_weekly_ok <- unique(dates_weekly_ok$week_year)

dates_daily_ok <- data_daily[above_bol_sup == 1,
                             .(Date, week_year, month_year)]

dates_daily_ok <- dates_daily_ok[week_year %in% uniques_dates_weekly_ok]

# CHECKLIST MARCHE --------------------------------------------------------

# Trend Daily CAC40 haussier (> MM200 + Loi de Dow)
# Noeud de MM CAC40 

tmp <- above_sma200(data_daily)


# Les meilleures zones d'achat sont en début de flux haussier court-terme du CAC40 
# soit wma12 > ema21 
# soit montreux > sma20 daily 

data_daily <- cac40_flux_haussier_ct(data_daily)

tmp <- data_daily[flux_haussier_ct == 1, .(Date, Close, flux_haussier_ct)]

data_daily <- atr(data_daily, n = 20)
data_daily <- atr(data_daily, n = 50)

data_daily <- noeud_mm(data_daily)

data_daily <- mavg_taille_bougie(data_daily, k = 3)

# Average True Range
data_daily <- data_daily[, atr :=  data.table(TTR::ATR(data_daily[,c("High","Low","Close")], n=4))[, atr]]
# True Range 
data_daily <- data_daily[, tr :=  data.table(TTR::ATR(data_daily[,c("High","Low","Close")], n=4))[, tr]]
# Distance between High & low 
data_daily <- data_daily[, taille_bougie := abs(Close - Open)]
data_daily <- data_daily[, mavg_taille_bougie := zoo::rollmean(taille_bougie, k = 3, fill = NA, align = "right")]
# Perf entre close et open 
data_daily <- data_daily[, perf_100 := (Close/Open -1)*100]

data_daily[!is.na(bol_sup), .(nb = .N), by = fifelse(Close > bol_sup, 1, 0)][, prop := nb/sum(nb)][]
# Descritpive statisitcs on performance 
skim(data_daily$perf_100)