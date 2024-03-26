# Wrapper functions 

# Create key for joins between monthly, weekly and daily data ----
create_key_joins <- function(data){
    
    dt <- copy(data)
    
    dt <- dt[, week_nb := isoweek(date)]
    
    dt <- dt[, week_year := str_c(week_nb, isoyear(date), sep = "_")]
    
    dt <- dt[, month := month(date)]
    
    dt <- dt[, month_year := str_c(month, year(date), sep = "_")]
    
    return(dt)
    
}

# Order by dates - ascending order ----
order_date <- function(data){

    dt <- copy(data)
    dt <- dt[, date := ymd(date)][order(date)]

    return(dt)
}
# Add week days

last_day <- function(data){

    dt <- copy(data)

    max_dates <- dt[, .(max_date = max(date)), by = month_year][, max_date]

    dt <- dt[date %in% max_dates, last_day_month := 1]

    max_dates <- dt[, .(max_date = max(date)), by = week_year][, max_date]

    dt <- dt[date %in% max_dates, last_day_week := 1]

    return(dt)

}

daily_perf <- function(data){

    dt <- copy(data)

    dt <- dt[, daily_perf := adj_close/data.table::shift(adj_close, n = 1)-1]

    return(dt)
}
# Add bollinger Bands ----
add_bollinger <- function(data, n, sd){

    dt <- copy(data)

    dt <- dt[, bol_inf := data.table(BBands(dt$adj_close, n = n, sd = sd))[, dn]]

    dt <- dt[, bol_sup := data.table(BBands(dt$adj_close, n = n, sd = sd))[, up]]

    dt <- dt[, mavg := data.table(BBands(dt$adj_close, n = n, sd = sd))[, mavg]]

    return(dt)

}

shrink_bol <- function(data, n){

    dt <- copy(data)

    dt <- dt[, sizebol := bol_sup - bol_inf]

    dt <- dt[,  rollmean_sizebol := frollmean(sizebol,
                                              n = n,
                                              fill = NA,
                                              align = "right")]

    dt <- dt[, shrink_bol := fifelse(sizebol < rollmean_sizebol, 1, 0)]

    return(dt)

}
# Add different moving averages ----
add_moving_averages <- function(data){

    dt <- copy(data)

    # WMA4 pour tendance court-terme : pour observer les différentes phases poussée/consolidation/congestion/poussée
    dt <- dt[, wma4 :=  WMA(dt$adj_close, n = 4)]
    dt <- dt[, wma12 :=  WMA(dt$adj_close, n = 12)]
    dt <- dt[, ema21 :=  EMA(dt$adj_close, n = 21)]
    dt <- dt[, sma10 :=  SMA(dt$adj_close, n = 10)]
    dt <- dt[, sma20 :=  SMA(dt$adj_close, n = 20)]
    dt <- dt[, sma50 :=  SMA(dt$adj_close, n = 50)]
    dt <- dt[, wma150 :=  WMA(dt$adj_close, n = 150)]

    if(nrow(dt) >= 200){
        dt <- dt[, sma200 :=  SMA(dt$adj_close, n = 200)]
    }

    return(dt)

}

# Guppy Multiple Moving Averages ---

guppy_moving_averages <- function(data){

    dt <- copy(data)

    # Short term moving averages - traders' behaviour
    dt <- dt[, ema3 :=  EMA(dt$adj_close, n = 3)]
    dt <- dt[, ema5 :=  EMA(dt$adj_close, n = 5)]
    dt <- dt[, ema8 :=  EMA(dt$adj_close, n = 8)]
    dt <- dt[, ema10 :=  EMA(dt$adj_close, n = 10)]
    dt <- dt[, ema12 :=  EMA(dt$adj_close, n = 12)]
    dt <- dt[, ema15:=  EMA(dt$adj_close, n = 15)]

    # Long term moving averages - investors' behaviour
    dt <- dt[, ema30 :=  EMA(dt$adj_close, n = 30)]
    dt <- dt[, ema35 :=  EMA(dt$adj_close, n = 35)]
    dt <- dt[, ema40 :=  EMA(dt$adj_close, n = 40)]
    dt <- dt[, ema45 :=  EMA(dt$adj_close, n = 45)]
    dt <- dt[, ema50 :=  EMA(dt$adj_close, n = 50)]
    dt <- dt[, ema60 :=  EMA(dt$adj_close, n = 60)]

    return(dt)
}

# Au dessus de la boll sup ----
above_bollinger <- function(data){

    dt <- copy(data)

    dt <- dt[!is.na(bol_sup),
             above_bol_sup := fifelse(adj_close > bol_sup, 1, 0)]

    return(dt)
}


# En dessous de la boll sup ----
below_bollinger_sup <- function(data){

    dt <- copy(data)

    dt <- dt[!is.na(bol_sup),
             below_bol_sup := fifelse(adj_close < bol_sup, 1, 0)]

    return(dt)
}




# En dessous de la boll inf ----
below_bollinger <- function(data){

    dt <- copy(data)

    dt <- dt[!is.na(bol_inf),
             below_bol_inf := fifelse(adj_close < bol_inf, 1, 0)]

    return(dt)
}

# Average True Range ----

atr <- function(data, n){

    dt <- copy(data)

    dt <- dt[,  (str_c("atr", n)) := data.table(TTR::ATR(dt[,c("high","low","adj_close")], n = n))[, atr]]

    return(dt)
}

# Noeud moyenne mobile - point de surveillance important !

noeud_mm_ct <- function(data){

    dt <- copy(data)

    dt <-dt[, tmp1 := pmax(wma12, ema21)]
    dt <-dt[, tmp2 := pmin(wma12, ema21)]
    dt <-dt[, tmp3 := pmax(tmp1, adj_close)]
    dt <-dt[, tmp4 := pmin(tmp2, adj_close)]
    dt <-dt[, tmp5 := pmax(tmp3, tmp4) - pmin(tmp3, tmp4)]

    dt <- dt[, noeud_mm_ct := fifelse(tmp5 < 0.5 * atr20, 1, 0)]

    return(dt)
}


noeud_mm_mt <- function(data){

    dt <- copy(data)

    dt <- dt[, tmp1 := pmax(wma12, ema21)]
    dt <- dt[, tmp2 := pmax(tmp1, sma50)]
    dt <- dt[, tmp3 := pmin(wma12, ema21)]
    dt <- dt[, tmp4 := pmin(tmp3, sma50)]

    dt <- dt[, noeud_mm_mt := fifelse((adj_close >= (tmp2 - atr20)) & ((tmp2-tmp4) < atr20), 1, 0)]

    return(dt)
}

# SMA200 haussiere

ma_up <- function(data,
                  ma,
                  k){

    dt <- copy(data)

    dt <- dt[, (str_c(ma, "mean", k, "periods", sep = "_")) := zoo::rollmean(get(ma), k = k, fill = NA, align = "right")]

    dt <- dt[, (str_c(ma, "up", sep = "_")) := fifelse(get(ma) > get(str_c(ma, "mean", k, "periods", sep = "_")), 1, 0)]
}


# Moyenne taille bougie sur k périodes - pour détecter la baisse de volatilité

mavg_taille_bougie <- function(data, k){

    dt <- copy(data)

    dt <- dt[, taille_bougie := abs(adj_close - open)]

    dt <- dt[, mavg_taille_bougie := zoo::rollmean(taille_bougie, k = k, fill = NA, align = "right")]

    return(dt)
}

petite_bougie <- function(data,
                          k){

    dt <- copy(data)

    dt <- dt[, median_taille_bougie := zoo::rollmedian(taille_bougie, k = k, fill = NA, align = "right")]

    # On considère comme petite bougie une bougie de taille inférieure à la taille médiane
    dt <- dt[, petite_bougie := fifelse(taille_bougie <= 0.8*median_taille_bougie, 1, 0)]

    dt <- dt[, petite_bougie_prev := data.table::shift(petite_bougie, n = 1)]

    dt <- dt[, petite_bougie_ante_prev := data.table::shift(petite_bougie, n = 2)]

    return(dt)
}

# vol_down <- function(data){
#
#     dt <- copy(data)
#
#     dt <- dt[, petites_bougies_consec := ]
# }
#

# Performance max sur ndays jours sur une période de temps donnée

get_start_end_periods <- function(data,
                                  horizon,
                                  ndays) {

    dt <- copy(data)

    dt <- dt[, .(start_date = frollapply(date,
                                         n = ndays,
                                         FUN = function(x) min(x),
                                         fill = NA,
                                         align = "right"),
                 end_date = frollapply(date,
                                       n = ndays,
                                       FUN = function(x) max(x),
                                       fill = NA,
                                       align = "right"))]

    dt <- dt[,  c("start_date",
                  "end_date") := list(as_date(start_date, origin = lubridate::origin),
                                      as_date(end_date, origin = lubridate::origin))]

    dt <- left_join(dt,
                    daily[,.(date, close)],
                    by = c("start_date" = "date"))

    setnames(dt, old = "close", new = "close_start_date")

    dt <- left_join(dt,
                    daily[,.(date, close)],
                    by = c("end_date" = "date"))

    setnames(dt, old = "close", new = "close_end_date")

    dt <- dt[, perf_n_days := close_end_date / close_start_date - 1]

    dt <- dt[, start_horizon := frollapply(end_date,
                                           n = horizon,
                                           FUN = function(x) min(x),
                                           fill = NA,
                                           align = "right")]

    dt <- dt[,  start_horizon := as_date(start_horizon, origin = lubridate::origin)]

    dt <- dt[!is.na(start_horizon)]

    dt <- dt[, actif := asset]

    return(dt)

}

max_perf_horizon <- function(start_horizon_val,
                             end_horizon_val) {


    max_perf_n_days <- res_horizon_n_days[end_date %between% c(ymd(start_horizon_val), ymd(end_horizon_val)), max(perf_n_days)]

    res <- data.table(actif = asset,
                      start_horizon = ymd(start_horizon_val),
                      end_horizon = ymd(end_horizon_val),
                      max_perf = max_perf_n_days)

    setnames(res, old = "max_perf", new =  str_c("max_perf", ndays, "days", sep = "_"))

    return(res)

}

# periods <- function(data,
#                     end_date,
#                     horizon,
#                     nb_days){
#
#     dt <- copy(data)
#
#     unique_dates <- unique(dt$date)
#
#     # Generate all dates
#     dates <- unique_dates[between(unique_dates,
#                                   end_date - horizon,
#                                   end_date)]
#
#     # Create a list to store the date combinations
#     date_combinations <- list()
#
#     date_combinations <- lapply(1:(length(dates)-(nb_days)), function(i) dates[i:(i+(nb_days))])
#
#     min_dates <- c(lapply(date_combinations, function(i) min(i)))
#
#     max_dates <- c(lapply(date_combinations, function(i) max(i)))
#
#     res <- data.table(min_dates, max_dates)
#
#     return(res)
# }
#
# # Renvoie la performance sur une période donnée - définie à partir des min_date et max_date
#
# perf_horizon_days <- function(data,
#                               min_date,
#                               max_date){
#
#     dt <- copy(data)
#
#     # min_date <- as.Date(min_date)
#     #
#     # max-date <- as.Date(max_date)
#
#     perf <- as.numeric(dt[date == max_date, adj_close]/dt[date == min_date, adj_close] -1)
#
#     return(data.table(min_date,
#                       max_date,
#                       perf))
#
# }

# Above sma200

above_ma <- function(data,
                     ma){

    dt <- copy(data)

    dt <- dt[, (str_c("above", ma, sep ="_")) := fifelse(adj_close > get(ma), 1, 0)]

    return(dt)
}


below_ma <- function(data,
                     ma,
                     atr = NULL){

    dt <- copy(data)

    if(!missing(atr)){

        dt <- dt[, (str_c("below", ma, sep ="_")) := fifelse(adj_close < get(ma) - get(atr), 1, 0)]

        dt <- dt[, (str_c(ma, "minus", atr, sep ="_")) := get(ma) - get(atr)]

    } else {

        dt <- dt[, (str_c("below", ma, sep ="_")) := fifelse(adj_close < get(ma), 1, 0)]
    }


    return(dt)
}


# Tester si le cours de cloture est supérieur à toutes les moyennes mobiles

above_all_ma <- function(data){

    dt <- copy(data)

    dt <-dt[, max_ma := pmax(wma12, ema21, sma50, wma150, sma200)]

    dt <- dt[, above_all_ma := fifelse(adj_close > max_ma, 1, 0)]

    return(dt)
}


# Guppy strategies
# Tester si les short term moving average sont toutes au dessus des long term moving averages

short_term_above_long_term  <- function(data){

    dt <- copy(data)

    dt <- dt[, min_short_term_ma := pmin(ema3, ema5, ema8, ema10, ema12, ema15)]

    dt <- dt[, max_short_term_ma := pmax(ema3, ema5, ema8, ema10, ema12, ema15)]

    dt <- dt[, min_long_term_ma := pmin(ema30, ema35, ema40, ema45, ema50, ema60)]

    dt <- dt[, max_long_term_ma := pmax(ema30, ema35, ema40, ema45, ema50, ema60)]

    dt <- dt[, short_term_above_long_term := fifelse(min_short_term_ma > max_long_term_ma, 1, 0)]

    dt <- dt[, short_term_below_long_term := fifelse(max_short_term_ma < min_long_term_ma, 1, 0)]

    return(dt)
}

ok_volume <- function(data,
                      avg_volume_crit = 300000){

    dt <- copy(data)

    dt <- dt[, ok_vol := fifelse(rollmean_close_volume > avg_volume_crit, 1, 0)]

    return(dt)
}
# Moyenne capital échangé sur n périodes

rollmean_close_volume <- function(data,
                                  n){

    dt <- copy(data)

    dt <- dt[, rollmean_close_volume := frollmean(adj_close * volume,
                                                  n = n,
                                                  fill = NA,
                                                  align = "right")]

    return(dt)

}

# Max capital échangé sur n périodes

rollmax_close_volume <- function(data,
                                 n){

    dt <- copy(data)

    dt <- dt[, rollmax_close_volume := frollapply(adj_close * volume,
                                                  n = n,
                                                  FUN = function(x) max(x),
                                                  fill = NA,
                                                  align = "right")]

    return(dt)
}

# Detecter si OPA

opa <- function(data,
                n,
                opa_seuil){

    dt <- copy(data)

    dt <- dt[, opa_high := frollapply(high,
                                      n = n,
                                      FUN = function(x) max(x),
                                      fill = NA,
                                      align = "right")]

    dt <- dt[, opa_low := frollapply(low,
                                     n = n,
                                     FUN = function(x) min(x),
                                     fill = NA,
                                     align = "right")]

    # Detecter si opa

    dt <- dt[, opa := fifelse(opa_high/opa_low < 1+opa_seuil, 1, 0)]

    return(dt)

}

# Stop loss fixe

sl_fixe <- function(data,
                    seuil){

    dt <- copy(data)

    dt <- dt[, sl_touche := fifelse(adj_close < prix_entree * (1-seuil), 1, 0)]

    return(dt)

}

# Keltner Channel - pour fixer stop loss

keltner_channel <- function(data,
                            n = 3,
                            atr = 1){

    dt <- copy(data)

    keltner <- keltnerChannels(dt[, .(high, low, adj_close)], n = n, atr = atr)

    keltner <- as.data.table(keltner)

    dt <- dt[, keltner_dn := keltner$dn]

    dt <- dt[, below_keltner_dn := fifelse(adj_close < keltner_dn, 1, 0)]

    return(dt)

}

# Amsterdam safeline ----

amsterdam_safeline <- function(data,
                               n = 150){

    dt <- copy(data)

    dt <- dt[, tmp1:= pmax(sma200, sma50)]

    dt <- dt[, tmp2:= pmax(wma12, ema21)]

    dt <- dt[, tmp3:= pmax(tmp1, tmp2)]

    dt <- dt[, highest := frollapply(adj_close,
                                     n = n,
                                     FUN = function(x) max(x),
                                     fill = NA,
                                     align = "right")]

    dt <- dt[, tmp4 := pmax(highest - 3*atr20, wma150)]

    dt <- dt[, safeline := pmax(tmp3, tmp4)]

    dt <- dt[, safeline_prev := data.table::shift(safeline, n = 1)]

    dt <- dt[safeline != safeline_prev , safelinelow := safeline - 1*atr20]

    dt <- dt[, safelinelow_prev := data.table::shift(safelinelow, n = 1)]

    dt <- dt[adj_close < safelinelow_prev & safeline > safeline_prev , safeline := safeline_prev]

    return(dt)

}

ok_amsterdam  <- function(data,
                          avg_volume_crit = 200000,
                          max_volume_crit = 500000){

    dt <- copy(data)

    # Trier sur critère de Volume moyen et Volume max
    dt <- dt[, ok_vol_amsterdam := fifelse(rollmean_close_volume > avg_volume_crit | rollmax_close_volume > max_volume_crit, 1, 0)]

    dt <- dt[, ok_amsterdam := fifelse(adj_close >= max_ma - atr50  &
                                           ok_vol_amsterdam == 1 &
                                           opa == 0,
                                       1,
                                       0)]


    return(dt)

}

relance_puissance <- function(data,
                              avg_volume_crit = 200000,
                              max_volume_crit = 500000){

    dt <- copy(data)

    dt <- dt[, ok_vol_amsterdam := fifelse(rollmean_close_volume > avg_volume_crit, 1, 0)]

    dt <- dt[, ok_relance := fifelse(ok_vol_amsterdam == 1 &
                                         # noeud_mm_ct == 1 &
                                         noeud_mm_mt == 1 &
                                         opa == 0 &
                                         above_sma200 == 1 &
                                         sma200_up == 1,
                                     1,
                                     0)]


    return(dt)

}


# Strategie basée sur la safeline ----

above_safeline <- function(data){

    dt <- copy(data)

    dt <- dt[, above_safeline := fifelse(adj_close > safeline, 1, 0)]

    return(dt)

}

below_safelinelow <- function(data){

    dt <- copy(data)

    dt <- dt[, below_safelinelow := fifelse(adj_close < safelinelow, 1, 0)]

    return(dt)

}

# Encadrer tendance autour de WMA150

frame_trend <- function(data,
                        n){

    dt <- copy(data)

    dt <- dt[, std := frollapply(adj_close,
                                 n = n,
                                 FUN = function(x) sd(x, na.rm = T),
                                 fill = NA,
                                 align = "right")]

    dt <- dt[, upper := wma150 + 1.5*std]

    dt <- dt[, lower := wma150 - 1*std]

    return(dt)

}

# Strategie basée sur la safeline ----

strategie_safeline <- function(data,
                               k_init = 1000){

    dt <- copy(data)

    dt <- dt[above_safeline == 1, entree_position_safeline := "entree"]

    dt <- dt[below_safelinelow == 1, sortie_position_safeline := "sortie"]

    row_entree <- data.table(row_number = dt[entree_position_safeline == "entree", which = TRUE], entree_sortie = "entree")

    row_sortie <- data.table(row_number = dt[sortie_position_safeline == "sortie", which = TRUE], entree_sortie = "sortie")

    row_entree_sortie <- rbind(row_entree, row_sortie)

    row_entree_sortie <- row_entree_sortie[order(row_number)]

    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" &
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" &
                                               prev_entree_sortie == "entree", signal := "sortie"]

    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]

    index_entree <- row_entree_sortie[signal == "entree", row_number]

    index_sortie <-  row_entree_sortie[signal == "sortie", row_number]

    tmp1 <- dt[index_entree][, signal := "entree"]

    tmp2 <- dt[index_sortie][, signal := "sortie"]

    res <- rbind(tmp1, tmp2)

    res <- res[order(date)]

    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]

    res <- res[signal == "sortie", perf := adj_close/prev_close - 1]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    return(list(res,
                vector_perf,
                data.table(date_debut_strategie = min(res$date) ,
                           date_fin_strategie = max(res$date),
                           perf_cumul = perf_cumul*100,
                           capital_final = k_final))
    )
}

# Stratégie Amsterdam Noeud MM

strategie_amsterdam <- function(data,
                                k_init = 1000){

    dt <- copy(data)

    dt <- dt[(noeud_mm_ct == 1 | noeud_mm_mt == 1) & above_sma200 == 1 & ok_vol == 1 & sma200_up == 1, entree_position_mm := "entree"]

    dt <- dt[wma4_up == 0, sortie_position_mm := "sortie_1"]

    row_entree <- data.table(row_number = dt[entree_position_mm == "entree", which = TRUE], entree_sortie = "entree")

    row_sortie <- data.table(row_number = dt[sortie_position_mm == "sortie", which = TRUE], entree_sortie = "sortie")

    row_entree_sortie <- rbind(row_entree, row_sortie)

    row_entree_sortie <- row_entree_sortie[order(row_number)]

    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" &
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" &
                                               prev_entree_sortie == "entree", signal := "sortie"]

    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]

    index_entree <- row_entree_sortie[signal == "entree", row_number]

    index_sortie <-  row_entree_sortie[signal == "sortie", row_number]

    tmp1 <- dt[index_entree][, signal := "entree"]

    tmp2 <- dt[index_sortie][, signal := "sortie"]

    res <- rbind(tmp1, tmp2)

    res <- res[order(date)]

    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]

    res <- res[signal == "sortie", perf := adj_close/prev_close - 1]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    summary <- data.table(strategie = "strategie_amsterdam",
                          date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(adj_close)],
                          prix_fin_strategie = res[, last(adj_close)],
                          perf_cumul = perf_cumul*100,
                          capital_final = k_final,
                          nb_annees = as.numeric(difftime(max(res$date), min(res$date), units = "days"))/365.25
    )

    summary <- summary[, perf_annualisee := (perf_cumul/100)^(1/nb_annees)-1]

    return(list(res,
                summary)
    )

}

# Score Montreux Trend Following

montreux_score <- function(data_daily,
                           data_weekly,
                           data_monthly,
                           date_analyse = NULL,
                           avg_volume_crit = NULL,
                           max_volume_crit = NULL,
                           vitesse_max = NULL,
                           vitesse_crit = NULL){

    dt_daily <- copy(data_daily)
    dt_weekly <- copy(data_weekly)
    dt_monthly <- copy(data_monthly)

    if(!missing(vitesse_max)) {

        date_analyse <- as.Date(date_analyse)

        dt_daily <- dt_daily[between(date, date_analyse - 100, date_analyse)]

        #  Vitesse - plus grosse variation en 20 jours sur une période de 100 jours

        days_periods <- periods(data = dt_daily,
                                end_date = date_analyse,
                                horizon = 100,
                                nb_days = 20)

        len <- nrow(days_periods)

        list_dt_daily <- replicate(n = len,
                                   expr = {dt_daily},
                                   simplify = F)

        tmp <- pmap_dfr(list(list_dt_daily,
                             days_periods$min_dates,
                             days_periods$max_dates),
                        perf_horizon_days)

        vitesse_max <- max(tmp$perf)

        dt_daily <- dt_daily[, vitesse := round(vitesse_max*100)]

        dt_daily <- dt_daily[, ok_montreux := fifelse(above_bol_sup == 1 &
                                                          vitesse >= vitesse_crit &
                                                          # ok_vol == 1 &
                                                          opa == 0,
                                                      1,
                                                      0)]

        # Score < 1000 points - Une valeur va être proche d'être au-dessus de toutes ses moyennes mobiles
        # Score > 1000 points - Au dessus de ses moyennes mobiles
        # Vitesse = plus grosse variation en 3 jours sur les 100 derniers jours

        # Daily OK
        dt_daily <- dt_daily[ok_montreux == 1 , score_montreux := fifelse(above_bol_sup == 1, vitesse + 1000, vitesse)]


    } else {

        dt_daily <- dt_daily[, vitesse := 0]

        dt_daily <- dt_daily[, ok_montreux := fifelse(above_bol_sup == 1 &
                                                          # ok_vol == 1 &
                                                          opa == 0,
                                                      1,
                                                      0)]

        # Score < 1000 points - Une valeur va être proche d'être au-dessus de toutes ses moyennes mobiles
        # Score > 1000 points - Au dessus de ses moyennes mobiles
        # Vitesse = plus grosse variation en 3 jours sur les 100 derniers jours

        # Daily OK
        dt_daily <- dt_daily[ok_montreux == 1 , score_montreux := fifelse(above_bol_sup == 1, vitesse + 1000, vitesse)]


    }

    # weeks_year_montreux_daily <- dt_daily[ok_montreux == 1, unique(week_year)]

    # weeks_year_montreux_daily_weekly <- dt_weekly[week_year %in% weeks_year_montreux_daily & above_bol_sup == 1,  unique(week_year)]

    # months_year_montreux_daily_weekly <- dt_monthly[week_year %in% weeks_year_montreux_daily & above_bol_sup == 1,  unique(month_year)]

    # Weekly OK
    # dt_daily <- dt_daily[week_year %in% weeks_year_montreux_daily_weekly, score_montreux := score_montreux + 2000]

    # Monthly OK
    # dt_daily <- dt_daily[month_year %in% months_year_montreux_daily_weekly, score_montreux := score_montreux + 4000]

    # Weekly OK
    dt_daily <- dt_daily[adj_close > bol_sup_weekly, score_montreux := score_montreux + 2000]

    # Monthly OK
    dt_daily <- dt_daily[adj_close > bol_sup_monthly, score_montreux := score_montreux + 4000]

    res_daily <- dt_daily[ok_montreux == 1, .(date, week_year, adj_close, bol_sup, above_bol_sup, bol_sup_weekly, bol_sup_monthly, opa, ok_vol, score_montreux)]

    # return(list(dt_daily,
    #             res_daily))

    return(vitesse_max)

}
# EN COURS DE DEV

strategie_montreux <- function(data_daily,
                               data_weekly = NULL,
                               data_monthly = NULL,
                               k_init = 1000){

    dt_daily <- copy(data_daily)

    # dt_monthly <- copy(data_monthly)

    # setnames(dt_monthly, old = "above_bol_sup", new = "above_bol_sup_monthly")

    # dt_daily <- left_join(dt_daily,
    #                       dt_monthly[, .(month_year, above_bol_sup_monthly)],
    #                       by = "month_year")

    # Mensuel et hebdo ok comme conditions d'entrée

    dt_daily <- dt_daily[score_montreux >= 6000 , entree_position_mm := "entree"]

    dt_daily <- dt_daily[wma4_up_monthly == 0 & last_day_month == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn_monthly == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_wma4 == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    row_entree <- data.table(row_number = dt_daily[entree_position_mm == "entree", which = TRUE], entree_sortie = "entree")

    row_sortie <- data.table(row_number = dt_daily[sortie_position_mm == "sortie", which = TRUE], entree_sortie = "sortie")

    row_entree_sortie <- rbind(row_entree, row_sortie)

    row_entree_sortie <- row_entree_sortie[order(row_number)]

    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" &
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" &
                                               prev_entree_sortie == "entree", signal := "sortie"]

    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]

    index_entree <- row_entree_sortie[signal == "entree", row_number]

    index_sortie <-  row_entree_sortie[signal == "sortie", row_number]

    tmp1 <- dt_daily[index_entree][, signal := "entree"]

    tmp2 <- dt_daily[index_sortie][, signal := "sortie"]

    res <- rbind(tmp1, tmp2)

    res <- res[order(date)]

    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]

    res <- res[signal == "sortie", perf := adj_close/prev_close - 1]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    summary <- data.table(strategie = "strategie_montreux",
                          date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(adj_close)],
                          prix_fin_strategie = res[, last(adj_close)],
                          perf_cumul = perf_cumul*100,
                          capital_final = k_final,
                          nb_annees = as.numeric(difftime(max(res$date), min(res$date), units = "days"))/365.25
    )

    summary <- summary[, perf_annualisee := (perf_cumul/100)^(1/nb_annees)-1]

    return(list(dt_daily,
                res,
                vector_perf,
                summary)
    )
}

# Stratégie bol sup  ------------------------------------------------------

strat_boll <- function(data_daily,
                       k_init = 1000){

    dt_daily <- copy(data_daily)

    dt_daily <- dt_daily[above_bol_sup == 1, entree_position_mm := "entree"]

    dt_daily <- dt_daily[below_bol_sup == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn_monthly == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_wma4 == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    row_entree <- data.table(row_number = dt_daily[entree_position_mm == "entree", which = TRUE], entree_sortie = "entree")

    row_sortie <- data.table(row_number = dt_daily[sortie_position_mm == "sortie", which = TRUE], entree_sortie = "sortie")

    row_entree_sortie <- rbind(row_entree, row_sortie)

    row_entree_sortie <- row_entree_sortie[order(row_number)]

    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" &
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" &
                                               prev_entree_sortie == "entree", signal := "sortie"]

    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]

    index_entree <- row_entree_sortie[signal == "entree", row_number]

    index_sortie <-  row_entree_sortie[signal == "sortie", row_number]

    tmp1 <- dt_daily[index_entree][, signal := "entree"]

    tmp2 <- dt_daily[index_sortie][, signal := "sortie"]

    res <- rbind(tmp1, tmp2)

    res <- res[order(date)]

    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]

    res <- res[signal == "sortie", perf := adj_close/prev_close - 1]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    summary <- data.table(strategie = "strategie_boll",
                          date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(adj_close)],
                          prix_fin_strategie = res[, last(adj_close)],
                          perf_cumul = perf_cumul*100,
                          capital_final = k_final,
                          nb_annees = as.numeric(difftime(max(res$date), min(res$date), units = "days"))/365.25
    )

    summary <- summary[, perf_annualisee := (perf_cumul/100)^(1/nb_annees)-1]

    return(list(dt_daily,
                res,
                vector_perf,
                summary)
    )
}


# # Stratégie SMA 200 + Stop loss 7% ------------------------------------------------------

strat_sma <- function(data_daily,
                      k_init = 1000){

    dt_daily <- copy(data_daily)

    dt_daily <- dt_daily[above_sma200 == 1, entree_position_mm := "entree"]

    dt_daily <- dt_daily[above_sma200 == 0, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn_monthly == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_keltner_dn == 1, sortie_position_mm := "sortie"]

    # dt_daily <- dt_daily[below_wma4 == 1 & last_day_month == 1, sortie_position_mm := "sortie"]

    row_entree <- data.table(row_number = dt_daily[entree_position_mm == "entree", which = TRUE], entree_sortie = "entree")

    row_sortie <- data.table(row_number = dt_daily[sortie_position_mm == "sortie", which = TRUE], entree_sortie = "sortie")

    row_entree_sortie <- rbind(row_entree, row_sortie)

    row_entree_sortie <- row_entree_sortie[order(row_number)]

    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" &
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]

    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" &
                                               prev_entree_sortie == "entree", signal := "sortie"]

    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]

    index_entree <- row_entree_sortie[signal == "entree", row_number]

    index_sortie <-  row_entree_sortie[signal == "sortie", row_number]

    tmp1 <- dt_daily[index_entree][, signal := "entree"]

    tmp2 <- dt_daily[index_sortie][, signal := "sortie"]

    res <- rbind(tmp1, tmp2)

    res <- res[order(date)]

    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]

    res <- res[signal == "sortie", perf := adj_close/prev_close - 1]

    if(last(res)[,signal] == "entree") {

        res <- res[-nrow(res)]
    }

    entrees_sorties <- data.table(date_entree = res[signal == "entree", date],
                                  date_sortie = res[signal == "sortie", date],
                                  prix_entree = res[signal == "entree", adj_close],
                                  prix_sortie = res[signal == "sortie", adj_close],
                                  sl_keltner = res[signal == "entree", keltner_dn])

    entrees_sorties <- entrees_sorties[, sl := prix_entree * 0.95]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    summary <- data.table(strategie = "strategie_sma",
                          date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(adj_close)],
                          prix_fin_strategie = res[, last(adj_close)],
                          perf_cumul = perf_cumul*100,
                          capital_final = k_final,
                          nb_annees = as.numeric(difftime(max(res$date), min(res$date), units = "days"))/365.25
    )

    summary <- summary[, perf_annualisee := (perf_cumul/100)^(1/nb_annees)-1]

    return(list(entrees_sorties,
                dt_daily,
                res,
                vector_perf,
                summary)
    )
}

flux_haussier  <- function(data){

    dt <- copy(data)

    dt <- dt[, flux_haussier := fifelse(wma12 > ema21, 1, 0)]

    return(dt)
}

distance_bol_sup <- function(data) {

    dt <- copy(data)

    dt <- dt[, distance_bol_sup := adj_close - bol_sup]

    return(dt)

}

# Trends récents Mensuels  ------------------------------------------------

trend_recent_monthly <- function(date_analyse,
                                 avg_volume_crit = 300000){

    dt <- copy(monthly)

    # Filter on the previous 6 months before date_analyse asset
    dt <- dt[date < date_analyse]
    dt <- tail(dt, 6)

    dt <- dt[, ok_vol := fifelse(rollmean_close_volume > avg_volume_crit, 1, 0)]

    dt <- dt[, ok_trend_recent := fifelse(ok_vol == 1 &
                                              opa == 0 &
                                              adj_close >= bol_sup,
                                          1,
                                          0)]

    dt <- dt[order(date)][, ok_trend_recent_cumul := cumsum(ok_trend_recent), by = rleid(ok_trend_recent == 0L)]

    nb_months <- dt[, last(ok_trend_recent_cumul)]

    vitesse <- all_max_perf_n_days_horizon[between(as.numeric(end_horizon - date_analyse), -3, 3),
                                           round(max(get(str_c("max_perf", ndays, "days", sep = "_"))), 2) * 100]

    score <- ifelse(between(nb_months, 1, 5), max(0, 6 - nb_months) * 1000 + vitesse, 0)
    

    res <- data.table(asset = asset,
                      date_analyse = date_analyse,
                      vitesse = vitesse,
                      score = score)

    return(res)

}

# trend_recent_mensuel(ymd("2012-08-01"))
