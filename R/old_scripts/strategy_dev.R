# Fonction pour repérer noeud de moyennes mobiles ----

data_daily <- data_daily[, wma4 :=  WMA(data_daily$Close, n = 4)]
data_daily <- data_daily[, wma12 :=  WMA(data_daily$Close, n = 12)]
data_daily <- data_daily[, ema21 :=  EMA(data_daily$Close, n = 21)]
data_daily <- data_daily[, sma10 :=  SMA(data_daily$Close, n = 10)]
data_daily <- data_daily[, sma50 :=  SMA(data_daily$Close, n = 50)]
data_daily <- data_daily[, wma150 :=  WMA(data_daily$Close, n = 150)]
data_daily <- data_daily[, sma200 :=  SMA(data_daily$Close, n = 200)]
data_daily <- data_daily[, lead_mavg_taille_bougie := shift(mavg_taille_bougie, n = 1)]

data_daily <-data_daily[, tmp1 := fifelse(wma12>ema21, wma12, ema21)]
data_daily <-data_daily[, tmp2 := fifelse(wma12<ema21, wma12, ema21)]
data_daily <-data_daily[, tmp3 := fifelse(tmp1>Close, tmp1, Close)]
data_daily <-data_daily[, tmp4 := fifelse(tmp2<Close, tmp2, Close)]
data_daily <-data_daily[, tmp5 := fifelse(tmp3>tmp4, tmp3, tmp4) - fifelse(tmp3<tmp4, tmp3, tmp4)]
data_daily <- data_daily[,  atr_20 := data.table(TTR::ATR(data_daily[,c("High","Low","Close")], n=20))[, atr]]
data_daily <- data_daily[, noeud_mm := fifelse(tmp5 < 0.5 * atr_20, 1, 0)]

mean_taille_bougie <- mean(data_daily$taille_bougie)

data_daily <- data_daily[, flux_haussier := fifelse(wma12 > ema21, 1, 0)]

data_daily <- data_daily[!is.na(sma200) &
                           taille_bougie < mean_taille_bougie & 
                           # taille_bougie < mavg_taille_bougie &
                           noeud_mm == 1 & 
                           Close > wma12 & 
                           Close > ema21 &
                           Close > sma50 &
                           Close > wma150 & 
                           Close > sma200, signal_entree_position := 1]

data_daily <- data_daily[, lead_open := shift(Open, n = -1)]

data_daily <- data_daily[signal_entree_position == 1 & !is.na(sma200) &
                           taille_bougie < mean_taille_bougie & 
                           # taille_bougie < mavg_taille_bougie &
                           noeud_mm == 1 & 
                           lead_open > wma12 & 
                           lead_open > ema21 &
                           lead_open > sma50 &
                           lead_open > wma150 & 
                           lead_open > sma200, entree_position := 1]

data_daily <- data_daily[entree_position == 1, prix_entree := lead_open]
data_daily <- data_daily[entree_position == 1, sl_3 := prix_entree * 0.97]

data_daily <- data_daily[!is.na(sma200) & 
                           Close < (wma12*0.97), sortie_position := 1]

data_daily <- data_daily[!is.na(sma200) & 
                           Close < (ema21*0.97), sortie_position := 1]

min_date <- data_daily[!is.na(prix_entree), min(Date)]+1
sl_3 <- data_daily[Date == min_date, sl_3]

data_daily <- data_daily[Date %between% c(min_date, max(Date)) , prix_sortie := fifelse(Low < sl_3, sl_3, 0)]
# Stratégie Amsterdam pour le CAC40 

tmp <- data_daily[, .(Date
                      , Open
                      , lead_open
                      , Close
                      , Low
                      , High
                      , taille_bougie
                      , mavg_taille_bougie
                      , lead_mavg_taille_bougie
                      , wma12
                      , ema21
                      , sma50
                      , wma150
                      , sma200
                      , noeud_mm
                      , flux_haussier
                      , signal_entree_position
                      , entree_position
                      , prix_entree
                      , sl_3
                      , sortie_position)]

tmp <- tmp[entree_position == 1 | sortie_50_position == 1 | sortie_100_position == 1]

tmp <- tmp[, lag_flux_haussier := shift(flux_haussier, n = -1)]
tmp <- tmp[, lead_flux_haussier := shift(flux_haussier, n = 1)]
tmp <- tmp[flux_haussier == 1 & is.na(lead_flux_haussier), buy_hold_sell := "buy"]
# tmp <- tmp[, lead_buy_hold_sell := shift(buy_hold_sell, n = 1)]
tmp <- tmp[flux_haussier == 1 & lead_flux_haussier == 1, buy_hold_sell :=  "hold"]
tmp <- tmp[is.na(flux_haussier) & lead_flux_haussier == 1, buy_hold_sell :=  "sell"]
tmp <- tmp[buy_hold_sell %in% c("buy", "sell")]
tmp <- tmp[, lead_close := shift(Close, n = 1)]
tmp <- tmp[buy_hold_sell == "sell", perf_points := Close-lead_close]
sum(tmp$perf_points, na.rm = T)

tmp <- data_daily[, .(Date, Close, taille_bougie, mavg_taille_bougie, noeud_mm)]

p_daily <- data_daily[, .(date = as.character(Date), Open, Close, Low, High, wma4, wma12, ema21, sma50)] |> 
  e_charts(date) |> 
  e_candle(opening = Close, closing = Open, low = Low, high = High) |> 
  e_line(wma4,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
  e_line(wma12,  showSymbol = FALSE) |> 
  e_line(ema21,  showSymbol = FALSE) |> 
  e_line(sma50,  showSymbol = FALSE) |> 
  # e_line(wma150,  showSymbol = FALSE) |> 
  # e_line(sma200,  showSymbol = FALSE) |> 
  # e_color(
  #   c("white", "white", "white", "grey", "red", "cyan")) |> 
  e_datazoom(type = "slider") |> 
  e_title("D", "EOD Historical data") |> 
  # e_theme("dark") |>  # theme
  e_tooltip(
    axisPointer = list(
      type = "cross"
    )
  ) 

