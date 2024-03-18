
# LOAD LIBRARIES ----------------------------------------------------------
library(alphavantager)
library(data.table)
library(tidyverse)
library(tidyquant)
library(TTR)
library(plotly)
library(echarts4r)
library(patchwork)
library(lubridate)
library(skimr)
library(timetk)
library(modeltime)
# av_get(av_fun = "SECTOR")

# SET API KEY - Alpha Vantage ---
av_api_key("CV5U6KY4Q05D4MN9")

# Stock prices 
# tmp <- av_get(symbol = "MSFT", av_fun = "TIME_SERIES_MONTHLY", outputsize = "full")
# other great ressources for finanacial data : www.eodhistoricaldata.com / 20 bucks a month 

# bitcoin_daily <- av_get(symbol = "BTC", av_fun = "DIGITAL_CURRENCY_DAILY", market = "CNY", outputsize = "full")
bitcoin_daily <- data.table(bitcoin_daily)
bitcoin_daily <- bitcoin_daily[, period := "daily"]
setnames(bitcoin_daily, c("open__usd_", "high__usd_", "low__usd_", "close__usd_"), c("open", "high", "low", "close"))
bitcoin_daily <- bitcoin_daily[, bol_inf := data.table(BBands(bitcoin_daily$close, n = 20, sd = 0.75))[, dn]]
bitcoin_daily <- bitcoin_daily[, bol_sup := data.table(BBands(bitcoin_daily$close, n = 20, sd = 0.75))[, up]]
bitcoin_daily <- bitcoin_daily[, mavg := data.table(BBands(bitcoin_daily$close, n = 20, sd = 0.75))[, mavg]]

# bitcoin_weekly <- av_get(symbol = "BTC", av_fun = "DIGITAL_CURRENCY_WEEKLY", market = "CNY", outputsize = "full")
bitcoin_weekly <- data.table(bitcoin_weekly)
bitcoin_weekly <- bitcoin_weekly[, period := "weekly"]
setnames(bitcoin_weekly, c("open__usd_", "high__usd_", "low__usd_", "close__usd_"), c("open", "high", "low", "close"))
bitcoin_weekly <- bitcoin_weekly[, bol_inf := data.table(BBands(bitcoin_weekly$close, n = 20, sd = 0.75))[, dn]]
bitcoin_weekly <- bitcoin_weekly[, bol_sup := data.table(BBands(bitcoin_weekly$close, n = 20, sd = 0.75))[, up]]
bitcoin_weekly <- bitcoin_weekly[, mavg := data.table(BBands(bitcoin_weekly$close, n = 20, sd = 0.75))[, mavg]]

# bitcoin_monthly <- av_get(symbol = "BTC", av_fun = "DIGITAL_CURRENCY_MONTHLY", market = "CNY", outputsize = "full")
bitcoin_monthly <- data.table(bitcoin_monthly)
bitcoin_monthly <- bitcoin_monthly[, period := "monthly"]
setnames(bitcoin_monthly, c("open__usd_", "high__usd_", "low__usd_", "close__usd_"), c("open", "high", "low", "close"))
bitcoin_monthly <- bitcoin_monthly[, bol_inf := data.table(BBands(bitcoin_monthly$close, n = 20, sd = 0.75))[, dn]]
bitcoin_monthly <- bitcoin_monthly[, bol_sup := data.table(BBands(bitcoin_monthly$close, n = 20, sd = 0.75))[, up]]
bitcoin_monthly <- bitcoin_monthly[, mavg := data.table(BBands(bitcoin_monthly$close, n = 20, sd = 0.75))[, mavg]]

bitcoin_prices <- rbind(bitcoin_daily,
                        bitcoin_weekly,
                        bitcoin_monthly)

saveRDS(bitcoin_prices, "bitcoin_prices.rds")

chartSeries(bitcoin_daily[, .(timestamp, open, close, low, high, volume)],
            theme=chartTheme('white'))

addBBands(n=20,sd=0.75)

bitcoin_prices_l <- bitcoin_prices[, .(timestamp, open, close, low, high, volume, bol_inf, bol_sup, mavg, period)]
bitcoin_prices_l |> 
  ggplot(aes(x = timestamp, y = close, group = period)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_line(aes(y=bol_inf), colour = "grey", linetype = 1) +
  geom_line(aes(y=bol_sup), colour = "grey", linetype = 1) +
  geom_line(aes(y=mavg), colour = "red", linetype = 2) +
  labs(title = "Bitcoin Bar Chart",
       subtitle = "MONTREUX",
       y = "Closing Price", x = "") +
  facet_wrap(~ period, ncol = 2)

min_price <- min(bitcoin_prices_l$close)
max_price <- max(bitcoin_prices_l$close)

bitcoin_prices_l |>
  e_charts(timestamp) |>
  e_candle(open, close, low, high) |> 
  e_y_axis(min = min_price, max = max_price)

bitcoin_prices_l <- data.table(bitcoin_prices_l)

p_daily <- bitcoin_prices_l[period == "daily", .(open, close, low, high, date = as.character(timestamp), bol_inf, bol_sup, mavg)] |> 
  e_charts(date) |> 
  e_candle(opening = close, closing = open, low = low, high = high, name = "Bitcoin") |> 
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_color(
    c("white", "white", "white")) |> 
  e_datazoom(type = "slider") |> 
  e_title("Candlestick chart", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

p_weekly <- bitcoin_prices_l[period == "weekly", .(open, close, low, high, date = as.character(timestamp), bol_inf, bol_sup, mavg)] |> 
  e_charts(date) |> 
  e_candle(opening = close, closing = open, low = low, high = high, name = "Bitcoin") |> 
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_color(
    c("white", "white", "white")) |> 
  e_datazoom(type = "slider") |> 
  e_title("Candlestick chart", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

p_monthly <- bitcoin_prices_l[period == "monthly", .(open, close, low, high, date = as.character(timestamp), bol_inf, bol_sup, mavg)] |> 
  e_charts(date) |> 
  e_candle(opening = close, closing = open, low = low, high = high, name = "Bitcoin") |> 
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_color(
    c("white", "white", "white")) |> 
  e_datazoom(type = "slider") |> 
  e_title("Candlestick chart", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

e_arrange(p_daily, p_weekly, p_monthly)


# Fetch data with EOD Historical data -------------------------------------
# https://eodhistoricaldata.com/

api.token <- "63d6a29a1877a4.36411178"
symbol <- "FCHI.INDX"
start_date <- "2000-01-01"
end_date <- "2023-01-31"

api_monthly <- paste("https://eodhistoricaldata.com/api/eod/", symbol, "?api_token=", api.token, "&period=m&order=d",  "&from=", start_date, "&to=", end_date, sep="")
api_weekly <- paste("https://eodhistoricaldata.com/api/eod/", symbol, "?api_token=", api.token, "&period=w&order=d",  "&from=", start_date, "&to=", end_date, sep="")
api_daily <- paste("https://eodhistoricaldata.com/api/eod/", symbol, "?api_token=", api.token, "&period=d&order=d",  "&from=", start_date, "&to=", end_date, sep="")

# Load data ----

# data_monthly <- read.csv(url(api_monthly))
# data_weekly <- read.csv(url(api_weekly))
# data_daily <- read.csv(url(api_daily))

# Save as RDS files ---- 
# saveRDS(data_monthly, str_c(symbol, "_monthly", ".rds"))
# saveRDS(data_weekly, str_c(symbol, "_weekly", ".rds"))
# saveRDS(data_daily, str_c(symbol, "_daily", ".rds"))

# Load RDS files
data_daily <- readRDS("FCHI.INDX_daily.rds")
data_weekly <- readRDS("FCHI.INDX_weekly.rds")
data_monthly <- readRDS("FCHI.INDX_monthly.rds")

data_daily <- as.data.table(data_daily)
data_weekly <- as.data.table(data_weekly)
data_monthly <- as.data.table(data_monthly)


# Order by dates - ascending order 
data_daily <- data_daily[, Date := ymd(Date)][order(Date)]
data_weekly <- data_weekly[, Date := ymd(Date)][order(Date)]
data_monthly <- data_monthly[, Date := ymd(Date)][order(Date)]

# Add bollinger Bnads with 0,75 std 
data_daily <- data_daily[, bol_inf := data.table(BBands(data_daily$Close, n = 20, sd = 0.75))[, dn]]
data_daily <- data_daily[, bol_sup := data.table(BBands(data_daily$Close, n = 20, sd = 0.75))[, up]]
data_daily <- data_daily[, mavg := data.table(BBands(data_daily$Close, n = 20, sd = 0.75))[, mavg]]
# MA4 pour tendance court-terme : pour observer les différentes phases poussée/consolidation/congestion/poussée 
data_daily <- data_daily[, wma4 :=  WMA(data_daily$Close, n = 4)]
data_daily <- data_daily[, wma12 :=  WMA(data_daily$Close, n = 12)]
data_daily <- data_daily[, ema21 :=  EMA(data_daily$Close, n = 21)]
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

data_weekly <- data_weekly[, bol_inf := data.table(BBands(data_weekly$Close, n = 20, sd = 0.75))[, dn]]
data_weekly <- data_weekly[, bol_sup := data.table(BBands(data_weekly$Close, n = 20, sd = 0.75))[, up]]
data_weekly <- data_weekly[, mavg := data.table(BBands(data_weekly$Close, n = 20, sd = 0.75))[, mavg]]
data_weekly <- data_weekly[, wma4 :=  WMA(data_weekly$Close, n = 4)]

data_monthly <- data_monthly[, bol_inf := data.table(BBands(data_monthly$Close, n = 20, sd = 0.75))[, dn]]
data_monthly <- data_monthly[, bol_sup := data.table(BBands(data_monthly$Close, n = 20, sd = 0.75))[, up]]
data_monthly <- data_monthly[, mavg := data.table(BBands(data_monthly$Close, n = 20, sd = 0.75))[, mavg]]
# 0.5 standard deviation pour anticiper la sortie de la bougie mensuelle de la boll sup 
data_monthly <- data_monthly[, bol_inf_0.5 := data.table(BBands(data_monthly$Close, n = 20, sd = 0.5))[, dn]]
data_monthly <- data_monthly[, bol_sup_0.5 := data.table(BBands(data_monthly$Close, n = 20, sd = 0.5))[, up]]
data_monthly <- data_monthly[, wma4 :=  WMA(data_monthly$Close, n = 4)]

# data_daily[, buy_hold_sell := fifelse(.I == min(which(open_close_greater_bol_sup == 1)), "buy","")]
# data_daily[, buy_hold_sell := fifelse(.I == min(which(open_close_lesser_mavg == 1)), "sell","")]
# plots 
p_daily <- data_daily[, .(Open, Close, Low, High, date = as.character(Date), bol_inf, bol_sup, mavg, wma4, wma12, ema21)] %>% 
  e_charts(Date) %>% 
  e_candle(opening = Open, closing = Close, low = Low, high = High) |> 
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_line(wma4,  showSymbol = FALSE) |> 
  e_line(wma12,  showSymbol = FALSE) |> 
  e_line(ema21,  showSymbol = FALSE) |> 
  e_color(
    c("white", "white", "white", "grey", "red", "cyan")) |> 
  e_datazoom(type = "slider") |> 
  e_title("D", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 


e_candle(e = data_daily[, .(Open, Close, Low, High, date = as.character(Date), bol_inf, bol_sup, mavg, wma4, wma12, ema21)],
         opening = Open, closing = Close, low = Low, high = High) 


p_weekly <- data_weekly[, .(Open, Close, Low, High, date = as.character(Date), bol_inf, bol_sup, mavg)] %>% 
  e_charts(date) %>% 
  e_candle(opening = Open, closing = Close, low = Low, high = High)
  e_candle(opening = Open, closing = Close, low = Low, high = High)  %>%  
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_color(
    c("white", "white", "white")) |> 
  e_datazoom(type = "slider") |> 
  e_title("W", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

p_monthly <- data_monthly[, .(Open, Close, Low, High, date = as.character(Date), bol_inf, bol_sup, mavg, bol_inf_0.5, bol_sup_0.5)] |> 
  e_charts(Date) |> 
  e_candle(opening = Open, closing = Close, low = Low, high = High) |> 
  e_line(bol_inf, showSymbol = FALSE) |> 
  e_line(bol_sup,  showSymbol = FALSE) |> 
  e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_line(bol_inf_0.5, showSymbol = FALSE) |> 
  e_line(bol_sup_0.5,  showSymbol = FALSE) |> 
  e_color(
    c("white", "white", "white", "cyan", "cyan", "cyan")) |> 
  e_datazoom(type = "slider") |> 
  e_title("M", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 


p_atr_daily <- data_daily[, .(date = as.character(Date), atr)][, median_atr := median(atr, na.rm = TRUE)] |> 
  e_charts(date) |> 
  e_line(atr, showSymbol = FALSE) |> 
  e_line(median_atr, showSymbol = FALSE) |>
  e_datazoom(type = "slider") |> 
  e_title("ATR DAILY", "Alpha Vantage data") |> 
  e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

e_arrange(p_monthly,p_weekly, p_daily, p_atr_daily)

# Bactest Montreux Strategy  ----------------------------------------------
# Trading Signals ----

# Critère 1 : les bougies M, W, D dépassent les boll sup. On regarde d'abord la bougie M, elle doit dépasser (ou sur le point de) dépasser la boll sup 0,75
# 4000 pts si close mensuel > boll sup 0,5
# 2000 pts si close weekly > boll sup 0,75
# 1000 pts si close daily > boll sup 0,75
# Critère 2 : Compression de la vol, bougies plus petites (ATR) 


# timeframe(daily)
# vitesse=round(highest[100](roc[20]))
# score=vitesse
# if close>=average[20]+0.75*std[20] then
# score=vitesse+1000
# endif
# okvolume=average[20](close*volume)>300000
# opa=highest[5](high)/lowest[5](low)<1.03
# timeframe(weekly)
# if close>=average[20]+0.75*std[20] then
# score=score+2000
# endif
# timeframe(monthly)
# if close>=average[20]+0.75*std[20] then
# score=score+4000
# endif
# ok=close>=average[20]+0.5*std[20] and vitesse>=15 and score>2000 and not opa

data_daily <- data_daily[Open > bol_sup | Close > bol_sup , open_close_greater_bol_sup := 1]
data_daily <- data_daily[Open < bol_inf | Close < bol_inf , open_close_lesser_wma4 := 1]
data_daily <- data_daily[wma12 > ema21 , flux_haussier := 1]

data_daily <- data_daily[, buy_hold_sell := fifelse(.I == min(which(flux_haussier == 1)), "buy", "")]
data_daily <- data_daily[, buy_hold_sell := fifelse((is.na(flux_haussier) & shift(buy_hold_sell, n = -1) == "buy"), "hold", "")]
data_daily <- data_daily[, flux_haussier_shifted := shift(flux_haussier, n = -1)]

tmp <- data_daily[, .(Date, Close, wma12, ema21, flux_haussier)]
tmp <- tmp[, buy_hold_sell := fifelse(.I == min(which(flux_haussier == 1)), "buy", "")]
tmp <- tmp[buy_hold_sell != "buy" & shift(flux_haussier, n = +1) == 1, buy_hold_sell :=  "hold"]
tmp <- tmp[buy_hold_sell %in% c("buy", "hold") & shift(flux_haussier, n = +1) == 1, buy_hold_sell :=  "hold"]

data_monthly <- data_monthly[, score := fifelse(Open > bol_sup | Close > bol_sup, 4000, 0)]
data_monthly <- data_monthly[, lag_score := shift(score, n = -1)]
data_monthly <- data_monthly[, lag_lag_score := shift(score, n = -2)]
data_monthly <- data_monthly[, lead_score := shift(score, n = 1)]
data_monthly <- data_monthly[, lead_lead_score := shift(score, n = 2)]

data_monthly <- data_monthly[, buy_hold_sell := fcase(score ==4000 & lead_score == 0, "buy",
                                                      score == 0 & lead_score == 4000, "sell")]

tmp <- data_monthly[buy_hold_sell %in% c("buy", "sell")]
tmp <- tmp[,lead_buy_hold_sell := shift(buy_hold_sell, n = 1)]
tmp <- tmp[,lead_close := shift(Close, n = 1)]
tmp <- tmp[, res := fifelse(buy_hold_sell == "sell" & lead_buy_hold_sell == "buy", Close - lead_close, 0)]
sum(tmp$res, na.rm = T)
