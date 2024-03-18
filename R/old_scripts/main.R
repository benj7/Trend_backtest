# Load packages ----
library(data.table)
library(lubridate)
library(zoo)
library(TTR)
library(stringr)
library(janitor)
library(purrr)
library(future)
library(furrr)
library(tictoc)
library(plotly)
library(timeDate)
library(tidyverse)

# SOURCE USEFUL FUNCTIONS ----
source("wrapper_functions.R")

# LOAD DATA ----

# CAC40 -
cac_daily <- readRDS("data/FCHI.INDX_daily.rds")
cac_daily <- as.data.table(cac_daily)

cac_weekly <- readRDS("data/FCHI.INDX_weekly.rds")
cac_weekly <- as.data.table(cac_weekly)

cac_monthly <- readRDS("data/FCHI.INDX_monthly.rds")
cac_monthly <- as.data.table(cac_monthly)

# Stocks -
apple_daily  <- fread("data/apple_daily.csv")
apple_weekly <- fread("data/apple_weekly.csv")
apple_monthly <- fread("data/apple_monthly.csv")

lvmh_daily <- fread("data/lmvh.csv")
valneva_daily <- fread("data/valneva.csv")

# CLEAN NAMES ----
cac_daily <- janitor::clean_names(cac_daily)
cac_weekly <- janitor::clean_names(cac_weekly)
cac_monthly <- janitor::clean_names(cac_monthly)

apple_daily <- janitor::clean_names(apple_daily)
apple_weekly <- janitor::clean_names(apple_weekly)
apple_monthly <- janitor::clean_names(apple_monthly)

lvmh_daily <- janitor::clean_names(lvmh_daily)
valneva_daily <- janitor::clean_names(valneva_daily)

# ORDER DATA ----
cac_daily <- order_date(cac_daily)
cac_weekly <- order_date(cac_weekly)
cac_monthly <- order_date(cac_monthly)

apple_daily <- order_date(apple_daily)
apple_weekly <- order_date(apple_weekly)
apple_monthly <- order_date(apple_monthly)


lvmh_daily <- order_date(lvmh_daily)
valneva_daily <- order_date(valneva_daily)

# Fetch the last day of each month 

cac_daily <- last_day(cac_daily)
apple_daily <- last_day(apple_daily)

# Add key joins 

cac_daily <- create_key_joins(cac_daily)
cac_weekly <- create_key_joins(cac_weekly)
cac_monthly <- create_key_joins(cac_monthly)


apple_daily <- create_key_joins(apple_daily)
apple_weekly <- create_key_joins(apple_weekly)
apple_monthly <- create_key_joins(apple_monthly)

# Add Bollinger Bands - 0.75* std(20)

cac_daily <- add_bollinger(cac_daily, n = 20, sd = 0.75)
cac_weekly <- add_bollinger(cac_weekly, n = 20, sd = 0.75)

apple_daily <- add_bollinger(apple_daily, n = 20, sd = 0.75)
apple_weekly <- add_bollinger(apple_weekly, n = 20, sd = 0.75)

# O.5 pour le mensuel pour détection précoce 
cac_monthly <- add_bollinger(cac_monthly, n = 20, sd = 0.5)
apple_monthly <- add_bollinger(apple_monthly, n = 20, sd = 0.5)

lvmh_daily <- add_bollinger(lvmh_daily, n = 20, sd = 0.75)
valneva_daily <- add_bollinger(valneva_daily, n = 20, sd = 0.75)

# Above Bollinger 

cac_daily <- above_bollinger(cac_daily)
cac_weekly <- above_bollinger(cac_weekly)
cac_monthly <- above_bollinger(cac_monthly)

apple_daily <- above_bollinger(apple_daily)
apple_weekly <- above_bollinger(apple_weekly)
apple_monthly <- above_bollinger(apple_monthly)

# ADD MOVING AVERAGES 

cac_daily <- add_moving_averages(cac_daily)
cac_weekly <- add_moving_averages(cac_weekly)
cac_monthly <- add_moving_averages(cac_monthly)

apple_daily <- add_moving_averages(apple_daily)
apple_weekly <- add_moving_averages(apple_weekly)
apple_monthly <- add_moving_averages(apple_monthly)

lvmh_daily <- add_moving_averages(lvmh_daily)
valneva_daily <- add_moving_averages(valneva_daily)

# ADD AVERAGE TRUE RANGE ----

cac_daily <- atr(cac_daily, n = 20)
cac_daily <- atr(cac_daily, n = 4)

apple_daily <- atr(apple_daily, n = 20)
apple_daily <- atr(apple_daily, n = 4)

cac_weekly <- atr(cac_weekly, n = 20)
cac_weekly <- atr(cac_weekly, n = 4)

apple_weekly <- atr(apple_weekly, n = 20)
apple_weekly <- atr(apple_weekly, n = 4)

cac_monthly <- atr(cac_monthly, n = 20)
cac_monthly <- atr(cac_monthly, n = 4)

apple_monthly <- atr(apple_monthly, n = 20)
apple_monthly <- atr(apple_monthly, n = 4)

lvmh_daily <- atr(lvmh_daily, n = 5)
lvmh_daily <- atr(lvmh_daily, n = 10)
lvmh_daily <- atr(lvmh_daily, n = 20)
lvmh_daily <- atr(lvmh_daily, n = 50)

valneva_daily <- atr(valneva_daily, n = 20)
valneva_daily <- atr(valneva_daily, n = 50)

# DETECTER NOEUD MOYENNE MOBILE ----

# Court terme - WMA12 / EMA21
cac_daily <- noeud_mm_ct(cac_daily)
cac_weekly <- noeud_mm_ct(cac_weekly)
cac_monthly <- noeud_mm_ct(cac_monthly)

apple_daily <- noeud_mm_ct(apple_daily)
apple_weekly <- noeud_mm_ct(apple_weekly)
apple_monthly <- noeud_mm_ct(apple_monthly)

lvmh_daily <- noeud_mm_ct(lvmh_daily)
valneva_daily <- noeud_mm_ct(valneva_daily)

# Moyen terme - WMA12 / EMA21 / SMA50 
cac_daily <- noeud_mm_mt(cac_daily)
cac_weekly <- noeud_mm_mt(cac_weekly)
cac_monthly <- noeud_mm_mt(cac_monthly)

apple_daily <- noeud_mm_mt(apple_daily)
apple_weekly <- noeud_mm_mt(apple_weekly)
apple_monthly <- noeud_mm_mt(apple_monthly)

lvmh_daily <- noeud_mm_mt(lvmh_daily)
valneva_daily <- noeud_mm_mt(valneva_daily)

# ABOVE MOVING AVERAGES----

# SMA 200 

cac_daily <- above_ma(cac_daily,
                      ma = "sma200")

cac_weekly <- above_ma(cac_weekly,
                       ma = "sma200")

cac_monthly <- above_ma(cac_monthly,
                        ma = "sma200")

apple_daily <- above_ma(apple_daily,
                        ma = "sma200")

apple_weekly <- above_ma(apple_weekly,
                         ma = "sma200")

# apple_monthly <- above_ma(apple_monthly,
#                           ma = "sma200")


lvmh_daily <- above_ma(lvmh_daily,
                       ma = "sma200")

valneva_daily <- above_ma(valneva_daily,
                          ma = "sma200")

apple_daily <- above_ma(apple_daily,
                        ma = "sma50")

lvmh_daily <- above_ma(lvmh_daily,
                       ma = "sma50")

valneva_daily <- above_ma(valneva_daily,
                          ma = "sma50")

# ALL MA 

cac_daily <- above_all_ma(cac_daily)
cac_weekly <- above_all_ma(cac_weekly)
cac_monthly <- above_all_ma(cac_monthly)

apple_daily <- above_all_ma(apple_daily)
apple_weekly <- above_all_ma(apple_weekly)
# apple_monthly <- above_all_ma(apple_monthly)

apple_daily <- above_all_ma(apple_daily)
lvmh_daily <- above_all_ma(lvmh_daily)
valneva_daily <- above_all_ma(valneva_daily)

# BELOW MOVING AVERAGES 

apple_daily <- below_ma(apple_daily,
                        ma = "sma50",
                        atr = "atr_20")

apple_daily <- below_ma(apple_daily,
                        ma = "sma50",
                        atr = "atr_20")

apple_monthly <- below_ma(apple_monthly,
                          ma = "wma4",
                          atr = "atr_4")

lvmh_daily <- below_ma(lvmh_daily,
                       ma = "sma50",
                       atr = "atr_20")

valneva_daily <- below_ma(valneva_daily,
                          ma = "sma50",
                          atr = "atr_20")

# SMA200 UP 
cac_daily <- ma_up(cac_daily,
                   ma = "sma200",
                   k = 5)

cac_weekly <- ma_up(cac_weekly,
                    ma = "sma200",
                    k = 5)

cac_monthly <- ma_up(cac_monthly,
                     ma = "sma200",
                     k = 5)

apple_daily <- ma_up(apple_daily,
                     ma = "sma200",
                     k = 5)

apple_weekly <- ma_up(apple_weekly,
                      ma = "sma200",
                      k = 5)

# apple_monthly <- ma_up(apple_monthly,
#                        ma = "sma200",
#                        k = 5)

cac_monthly <- ma_up(cac_monthly,
                     ma = "wma4",
                     k = 3)

apple_monthly <- ma_up(apple_monthly,
                       ma = "wma4",
                       k = 4)


lvmh_daily <- ma_up(lvmh_daily,
                    ma = "sma200",
                    k = 5)

valneva_daily <- ma_up(valneva_daily,
                       ma = "sma200",
                       k = 5)

# TAILLE MOYENNE DES BOUGIES SUR 20 PERIODES GLISSANTES 

cac_daily <- mavg_taille_bougie(cac_daily,
                                k = 40)

cac_weekly <- mavg_taille_bougie(cac_weekly,
                                 k = 40)

cac_monthly <- mavg_taille_bougie(cac_monthly,
                                  k = 40)

apple_daily <- mavg_taille_bougie(apple_daily,
                                  k = 40)

apple_weekly <- mavg_taille_bougie(apple_weekly,
                                   k = 40)

apple_monthly <- mavg_taille_bougie(apple_monthly,
                                    k = 40)

lvmh_daily <- mavg_taille_bougie(lvmh_daily,
                                 k = 40)

valneva_daily <- mavg_taille_bougie(valneva_daily,
                                    k = 40)

cac_daily <- petite_bougie(cac_daily,
                           k = 40)

cac_weekly <- petite_bougie(cac_weekly,
                            k = 40)

cac_monthly <- petite_bougie(cac_monthly,
                             k = 40)

apple_daily <- petite_bougie(apple_daily,
                             k = 40)

apple_weekly <- petite_bougie(apple_weekly,
                              k = 40)

apple_monthly <- petite_bougie(apple_monthly,
                               k = 40)

lvmh_daily <- petite_bougie(lvmh_daily,
                            k = 40)

valneva_daily <- petite_bougie(valneva_daily,
                               k = 40)

# MOYENNE CAPITAL ECHANGE SUR 20 PERIODES ----

cac_daily <- rollmean_close_volume(cac_daily, 
                                   n = 20)

cac_weekly <- rollmean_close_volume(cac_weekly, 
                                    n = 20)

cac_monthly <- rollmean_close_volume(cac_monthly, 
                                     n = 20)

apple_daily <- rollmean_close_volume(apple_daily, 
                                     n = 20)

apple_weekly <- rollmean_close_volume(apple_weekly, 
                                      n = 20)

apple_monthly <- rollmean_close_volume(apple_monthly, 
                                       n = 20)

lvmh_daily <- rollmean_close_volume(lvmh_daily, 
                                    n = 20)

valneva_daily <- rollmean_close_volume(valneva_daily, 
                                       n = 20)

# MAX CAPITAL ECHANGE SUR 20 PERIODES 

cac_daily <- rollmax_close_volume(cac_daily, 
                                  n = 20)

cac_weekly <- rollmax_close_volume(cac_weekly, 
                                   n = 20)

cac_monthly <- rollmax_close_volume(cac_monthly, 
                                    n = 20)

apple_daily <- rollmax_close_volume(apple_daily, 
                                    n = 20)

apple_weekly <- rollmax_close_volume(apple_weekly, 
                                     n = 20)

apple_monthly <- rollmax_close_volume(apple_monthly, 
                                      n = 20)

lvmh_daily <- rollmax_close_volume(lvmh_daily, 
                                   n = 20)

valneva_daily <- rollmax_close_volume(valneva_daily, 
                                      n = 20)
# OPA 

cac_daily <- opa(cac_daily, 
                 n = 5, 
                 opa_seuil = 0/100)

cac_weekly <- opa(cac_weekly, 
                  n = 5, 
                  opa_seuil = 0/100)

cac_monthly <- opa(cac_monthly, 
                   n = 5, 
                   opa_seuil = 0/100)

apple_daily <- opa(apple_daily, 
                   n = 5, 
                   opa_seuil = 3/100)

apple_weekly <- opa(apple_weekly, 
                    n = 5, 
                    opa_seuil = 3/100)

apple_monthly <- opa(apple_monthly, 
                     n = 5, 
                     opa_seuil = 3/100)

lvmh_daily <- opa(lvmh_daily, 
                  n = 5, 
                  opa_seuil = 3/100)

valneva_daily <- opa(valneva_daily, 
                     n = 5, 
                     opa_seuil = 3/100)

# Check volume 

cac_daily <- ok_volume(cac_daily)

apple_daily <- ok_volume(apple_daily)

lvmh_daily <- ok_volume(lvmh_daily)

valneva_daily <- ok_volume(valneva_daily)

# Safeline 

apple_daily <- amsterdam_safeline(data = apple_daily)
cac_daily <- amsterdam_safeline(data = cac_daily)
valneva_daily <- amsterdam_safeline(data = valneva_daily)
lvmh_daily <- amsterdam_safeline(data = lvmh_daily)

# Above/Below safeline

apple_daily <- above_safeline(data = apple_daily)
cac_daily <- above_safeline(data = cac_daily)
valneva_daily <- above_safeline(data = valneva_daily)
lvmh_daily <- above_safeline(data = lvmh_daily)

apple_daily <- below_safelinelow(data = apple_daily)
cac_daily <- below_safelinelow(data = cac_daily)
valneva_daily <- below_safelinelow(data = valneva_daily)
lvmh_daily <- below_safelinelow(data = lvmh_daily)

# TEST AMSTERDAM STRATEGY ----

