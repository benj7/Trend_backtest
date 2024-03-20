# LOAD PACKAGES ----
library(data.table)
library(lubridate)
library(magrittr)
library(zoo)
library(TTR)
library(stringr)
library(janitor)
library(purrr)
library(future)
library(furrr)
library(tictoc)
# library(plotly) not used 
library(timeDate)
library(tidyverse)
# library(ggplot2) not used 
# library(echarts4r) not used 
library(highcharter)
library(RColorBrewer)
# library(dygraphs) not used 
library(xts)
library(plotly)
library(htmlwidgets)

# SOURCE USEFUL FUNCTIONS ----
source("R/wrapper_functions.R")
source("R/highcharter_charts.R")

# LOAD DATA ----

assets <- c("sp",
            "cac",
            
            "atos",
            "adp",
            "axa",
            "orange",
            "lvmh",
            "valneva",
            "apple")

asset <- sample(assets, 1)

tic()
asset <- "atos"
daily <- fread(str_c("input/data/", asset, "_daily.csv"))
weekly <- fread(str_c("input/data/", asset, "_weekly.csv"))
monthly <- fread(str_c("input/data/", asset, "_monthly.csv"))

# CLEAN NAMES ----

daily <- janitor::clean_names(daily)
weekly <- janitor::clean_names(weekly)
monthly <- janitor::clean_names(monthly)

# CHECK FORMAT CONSISTENCY 

date_cols <- c("date")
num_cols <- c("open", "high", "low", "close", "adj_close", "volume", "dividends", "stock_splits")

daily <- daily[, (date_cols) := lapply(.SD, function(x) as.Date(x)), .SDcols = date_cols]
daily <- daily[, (num_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = num_cols]

weekly <- weekly[, (date_cols) := lapply(.SD, function(x) as.Date(x)), .SDcols = date_cols]
weekly <- weekly[, (num_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = num_cols]

monthly <- monthly[, (date_cols) := lapply(.SD, function(x) as.Date(x)), .SDcols = date_cols]
monthly <- monthly[, (num_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = num_cols]

# ORDER DATA ----
daily <- order_date(daily)
weekly <- order_date(weekly)
monthly <- order_date(monthly)

# Perf quotidienne 

daily <- daily_perf(daily)

# Fetch the last day of each month 

daily <- last_day(daily)

# Add key joins 

daily <- create_key_joins(daily)
weekly <- create_key_joins(weekly)
monthly <- create_key_joins(monthly)

# Add Bollinger Bands - 0.75* std(20)

daily <- add_bollinger(daily, n = 20, sd = 0.75)
daily <- shrink_bol(daily, n = 10)

weekly <- add_bollinger(weekly, n = 20, sd = 0.75)

# éventuellement O.5 pour le mensuel pour détection précoce 
monthly <- add_bollinger(monthly, n = 20, sd = 0.75)

# Above Bollinger 

daily <- above_bollinger(daily)
weekly <- above_bollinger(weekly)
monthly <- above_bollinger(monthly)

daily <- below_bollinger_sup(daily)
weekly <- below_bollinger_sup(weekly)
monthly <- below_bollinger_sup(monthly)

# Distance between adjusted close and bolinger upper 

daily <- distance_bol_sup(daily)
weekly <- distance_bol_sup(weekly)
monthly <- distance_bol_sup(monthly)

# ADD MOVING AVERAGES 

daily <- add_moving_averages(daily)
weekly <- add_moving_averages(weekly)
monthly <- add_moving_averages(monthly)

# Guppy Multiple Moving Averages 

daily <- guppy_moving_averages(daily)

# Short term above Long term 

daily <- short_term_above_long_term(daily)

# ADD AVERAGE TRUE RANGE ----

daily <- atr(daily, n = 50)
daily <- atr(daily, n = 20)
daily <- atr(daily, n = 4)

weekly <- atr(weekly, n = 20)
weekly <- atr(weekly, n = 4)

monthly <- atr(monthly, n = 20)
monthly <- atr(monthly, n = 4)
monthly <- atr(monthly, n = 1)

# Stop loss - Keltner Channel 

daily <- keltner_channel(daily)
weekly <- keltner_channel(weekly)
monthly <- keltner_channel(monthly)

# DETECTER NOEUD MOYENNE MOBILE ----

# Court terme - WMA12 / EMA21
daily <- noeud_mm_ct(daily)
weekly <- noeud_mm_ct(weekly)
monthly <- noeud_mm_ct(monthly)

# Moyen terme - WMA12 / EMA21 / SMA50 
daily <- noeud_mm_mt(daily)
weekly <- noeud_mm_mt(weekly)
monthly <- noeud_mm_mt(monthly)

# ABOVE MOVING AVERAGES----

# SMA 200 

daily <- above_ma(daily,
                  ma = "sma200")

weekly <- above_ma(weekly,
                   ma = "sma200")

# monthly <- above_ma(monthly,
#                     ma = "sma200")
# ALL MA 

daily <- above_all_ma(daily)
weekly <- above_all_ma(weekly)
# monthly <- above_all_ma(monthly)

# BELOW MOVING AVERAGES 

daily <- below_ma(daily,
                  ma = "sma200")

monthly <- below_ma(monthly,
                    ma = "wma4")

daily <- below_ma(daily,
                  ma = "sma50",
                  atr = "atr20")

monthly <- below_ma(monthly,
                    ma = "wma4")

daily <- below_ma(daily,
                  ma = "sma20")

weekly <- below_ma(weekly,
                   ma = "sma20")

monthly <- below_ma(monthly,
                    ma = "sma20")
# SMA200 UP 
daily <- ma_up(daily,
               ma = "sma200",
               k = 5)

weekly <- ma_up(weekly,
                ma = "sma200",
                k = 5)

# monthly <- ma_up(monthly,
#                  ma = "sma200",
#                  k = 5)

daily <- ma_up(daily,
               ma = "wma4",
               k = 5)

weekly <- ma_up(weekly,
                ma = "wma4",
                k = 3)

monthly <- ma_up(monthly,
                 ma = "wma4",
                 k = 3)


# TAILLE MOYENNE DES BOUGIES SUR 20 PERIODES GLISSANTES 
# La taille des bougies n'inclut pas les mèches

daily <- mavg_taille_bougie(daily,
                            k = 10)

weekly <- mavg_taille_bougie(weekly,
                             k = 4)

monthly <- mavg_taille_bougie(monthly,
                              k = 3)

daily <- petite_bougie(daily,
                       k = 10)

weekly <- petite_bougie(weekly,
                        k = 4)

monthly <- petite_bougie(monthly,
                         k = 3)

# MOYENNE CAPITAL ECHANGE SUR 20 PERIODES ----

daily <- rollmean_close_volume(daily, 
                               n = 20)

weekly <- rollmean_close_volume(weekly, 
                                n = 20)

monthly <- rollmean_close_volume(monthly, 
                                 n = 20)

# MAX CAPITAL ECHANGE SUR 20 PERIODES 

daily <- rollmax_close_volume(daily, 
                              n = 20)

weekly <- rollmax_close_volume(weekly, 
                               n = 20)

monthly <- rollmax_close_volume(monthly, 
                                n = 20)

# OPA 

daily <- opa(daily, 
             n = 5, 
             opa_seuil = 3/100)

weekly <- opa(weekly, 
              n = 5, 
              opa_seuil = 3/100)

monthly <- opa(monthly, 
               n = 5, 
               opa_seuil = 3/100)

# Check volume 

daily <- ok_volume(daily)

# Safeline 

daily <- amsterdam_safeline(data = daily)

# Above/Below safeline

daily <- above_safeline(data = daily)
daily <- below_safelinelow(data = daily)

# Frame the trend 

daily <- frame_trend(daily, 
                     150)

# FLux haussier 

daily <- flux_haussier(daily)

# On ajoute les bolligner des unités supérieures aux dannées daily 

setnames(weekly, old = "bol_sup", new = "bol_sup_weekly")

daily <- left_join(daily, 
                   weekly[!is.na(bol_sup_weekly), .(week_year, bol_sup_weekly)],
                   by = "week_year")

setnames(weekly, old = "bol_sup_weekly", new = "bol_sup")

setnames(monthly, old = "bol_sup", new = "bol_sup_monthly")

daily <- left_join(daily, 
                   monthly[, .(month_year, bol_sup_monthly)],
                   by = "month_year")

setnames(monthly, old = "bol_sup_monthly", new = "bol_sup")

setnames(weekly, old = "wma4_up", new = "wma4_up_weekly")
setnames(weekly, old = "wma4", new = "wma4_weekly")

daily <- left_join(daily,
                   weekly[, .(week_year, wma4_weekly, wma4_up_weekly)],
                   by = "week_year")

setnames(weekly, old = "wma4_up_weekly", new = "wma4_up")
setnames(weekly, old = "wma4_weekly", new = "wma4")


setnames(monthly, old = "wma4_up", new = "wma4_up_monthly")
setnames(monthly, old = "wma4", new = "wma4_monthly")

daily <- left_join(daily,
                   monthly[, .(month_year, wma4_monthly, wma4_up_monthly)],
                   by = "month_year")

setnames(monthly, old = "wma4_up_monthly", new = "wma4_up")
setnames(monthly, old = "wma4_monthly", new = "wma4")

setnames(monthly, old = "below_keltner_dn", new = "below_keltner_dn_monthly")

daily <- left_join(daily,
                   monthly[, .(month_year, below_keltner_dn_monthly)],
                   by = "month_year")

setnames(monthly, old = "below_keltner_dn_monthly", new = "below_keltner_dn")

# Ajout des SMA20 weekly 
setnames(weekly, old = "sma20", new = "sma20_weekly")

daily <- left_join(daily, 
                   weekly[, .(week_year, sma20_weekly)],
                   by = "week_year")

setnames(weekly, old = "sma20_weekly", new = "sma20")

# Ajout des SMA20 monthly 
setnames(monthly, old = "sma20", new = "sma20_monthly")

daily <- left_join(daily, 
                   monthly[, .(month_year, sma20_monthly)],
                   by = "month_year")

setnames(monthly, old = "sma20_monthly", new = "sma20")

# Ajout distances bol sup weekly 

setnames(weekly, old = "distance_bol_sup", new = "distance_bol_sup_weekly")

daily <- left_join(daily, 
                   weekly[, .(week_year, distance_bol_sup_weekly)],
                   by = "week_year")

setnames(monthly, old = "distance_bol_sup_weekly", new = "distance_bol_sup")

# et monthly 
setnames(monthly, old = "distance_bol_sup", new = "distance_bol_sup_monthly")

daily <- left_join(daily, 
                   monthly[, .(month_year, distance_bol_sup_monthly)],
                   by = "month_year")

setnames(monthly, old = "distance_bol_sup_monthly", new = "distance_bol_sup")

# Filtres stratégies Amsterdam 

daily <- ok_amsterdam(daily)

daily <- relance_puissance(daily)

# Performance max sur 5 jours sur chaque horizon de 100 jours 

ndays <- 5

res_horizon_n_days <- get_start_end_periods(daily, 
                                            horizon = 100,
                                            ndays = ndays)

if(!file.exists(str_c("output/data/", asset, "_all_max_perf_5_days_horizon.rds"))){
    
    # len <- nrow(res_horizon_n_days)
    # 
    # list_dt<- replicate(n = len,
    #                     expr = {res_horizon_5_days},
    #                     simplify = F)
    plan(multisession, workers =  parallelly::availableCores())
    
    all_max_perf_n_days_horizon <- future_pmap_dfr(list(res_horizon_n_days$start_horizon,
                                                        res_horizon_n_days$end_date),
                                                   max_perf_horizon)
    
    all_max_perf_n_days_horizon <- all_max_perf_n_days_horizon[order(-get(str_c("max_perf", ndays, "days", sep = "_")))]
    
    saveRDS(all_max_perf_n_days_horizon, str_c("output/data/", asset, "_all_max_perf_5_days_horizon.rds"))
}

# Reperer les trends mensuels récents ----

ndays <- 20

res_horizon_n_days <- get_start_end_periods(daily, 
                                            horizon = 100,
                                            ndays = ndays)


plan(multisession, workers =  parallelly::availableCores())

all_max_perf_n_days_horizon <- future_pmap_dfr(list(res_horizon_n_days$start_horizon,
                                                    res_horizon_n_days$end_date),
                                               max_perf_horizon)

all_max_perf_n_days_horizon <- all_max_perf_n_days_horizon[order(-get(str_c("max_perf", ndays, "days", sep = "_")))]

saveRDS(all_max_perf_n_days_horizon, str_c("output/data/", asset, "_all_max_perf_20_days_horizon.rds"))

unique_dates <- unique(tail(monthly, nrow(monthly) - 20)$date)

plan(multisession, workers =  parallelly::availableCores())

trend_mensuel_recent <- future_pmap_dfr(list(unique_dates),
                                        trend_recent_monthly)

trend_mensuel_recent <- left_join(trend_mensuel_recent,
                                  monthly[, .(date, adj_close, sma20)],
                                  by = c("date_analyse" = "date"))

saveRDS(trend_mensuel_recent, str_c("output/data/", asset, "_trend_mensuel.rds"))

# SAVE MODIFIED DATA ------------------------------------------------------

saveRDS(daily, str_c("output/data/", asset, "_daily_modified.rds"))
saveRDS(weekly, str_c("output/data/", asset, "_weekly_modified.rds"))
saveRDS(monthly, str_c("output/data/", asset, "_monthly_modified.rds"))

print(asset)

# TRENDFOLLOWING & DAILY PLOTS --------------------------------------------

p_trend <- plot_trendfollowing(plot_bollinger = TRUE,
                               plot_ma_ct = TRUE)

htmlwidgets::saveWidget(p_trend, str_c("output/charts/", asset, "_trend.html"))

p_daily <- plot_daily(plot_ma_ct = TRUE,
                      plot_ma_lt = TRUE,
                      plot_safeline = TRUE,
                      plot_frametrend = TRUE,
                      plot_atr = TRUE)

htmlwidgets::saveWidget(p_daily, str_c("output/charts/", asset, "_daily.html"))

toc()
