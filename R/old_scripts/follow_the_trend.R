# Aller dans le sens de la tendance ! 

# On calcule la performance tant que Close > SMA200 
# Prise et Sortie de position lorsque l'on croise 
# Load RDS files
data_daily_trend <- readRDS("FCHI.INDX_daily.rds")
data_daily_trend <- as.data.table(data_daily_trend)
data_daily_trend <- order_date(data_daily_trend)

data_daily_trend <- add_moving_averages(data_daily_trend)

data_daily_trend <- data_daily_trend[, above_sma200 := fifelse(Close > sma200, 1, 0)]
data_daily_trend <- data_daily_trend[, below_sma200 := fifelse(Close < sma200, 1, 0)]

data_daily_trend <- data_daily_trend[above_sma200 == 1, entree_position_sma200 := "entree"]
data_daily_trend <- data_daily_trend[below_sma200 == 1, sortie_position_sma200 := "sortie"]

data_daily_trend <- data_daily_trend[, shift_entree_position := shift(entree_position_sma200, n = 1)]

data_daily_trend <- data_daily_trend[entree_position_sma200 == "entree" &
                                         is.na(shift_entree_position), entree_sortie := "entree"]

data_daily_trend <- data_daily_trend[sortie_position_sma200 == "sortie" &
                                         shift_entree_position=="entree", entree_sortie := "sortie"]


data_daily_trend_lim <- data_daily_trend[entree_sortie %in% c("entree", "sortie"), .(Date, Close, entree_sortie)]

data_daily_trend_lim <- data_daily_trend_lim[, shift_close := shift(Close, n = 1)]
data_daily_trend_lim <- data_daily_trend_lim[entree_sortie == "sortie", perf := Close/shift_close -1]

min_date_sortie <- data_daily_trend_lim[entree_sortie == "sortie", min(Date)]

data_daily_trend_lim <- data_daily_trend_lim[Date == min_date_sortie, capital := 1000]
data_daily_trend_lim <- data_daily_trend_lim[entree_sortie == "sortie"]
data_daily_trend_lim <- data_daily_trend_lim[, capital := capital*(1+shift(perf))]
sum(data_daily_trend_lim$capital, na.rm = T)

# Above all moving averages 

data_daily_trend <- readRDS("FCHI.INDX_daily.rds")
data_daily_trend <- as.data.table(data_daily_trend)
data_daily_trend <- order_date(data_daily_trend)
data_daily_trend <- add_moving_averages(data_daily_trend)

data_daily_trend <- above_ma(data_daily_trend)

below_max_ma <- function(data){
    
    dt <- copy(data)
    
    dt <- dt[, below_max_ma := fifelse(Close < max_ma, 1, 0)]
    
    return(dt)
}

data_daily_trend <- below_max_ma(data_daily_trend)

data_daily_trend <- data_daily_trend[above_ma == 1, entree_position_all_ma := "entree"]
data_daily_trend <- data_daily_trend[below_max_ma == 1, sortie_position_all_ma := "sortie"]

data_daily_trend <- data_daily_trend[, shift_entree_position_all_ma := shift(entree_position_all_ma, n = 1)]

data_daily_trend <- data_daily_trend[entree_position_all_ma == "entree" &
                                         is.na(shift_entree_position_all_ma), entree_sortie_all_ma := "entree"]

data_daily_trend <- data_daily_trend[sortie_position_all_ma == "sortie" &
                                         shift_entree_position_all_ma=="entree", entree_sortie_all_ma := "sortie"]


data_daily_trend_lim <- data_daily_trend[entree_sortie_all_ma %in% c("entree", "sortie"), .(Date, Close, entree_sortie_all_ma)]

data_daily_trend_lim <- data_daily_trend_lim[, shift_close := shift(Close, n = 1)]
data_daily_trend_lim <- data_daily_trend_lim[entree_sortie_all_ma == "sortie", perf := Close/shift_close -1]

data_daily_trend_lim <- data_daily_trend_lim[entree_sortie_all_ma == "sortie", capital := 1000 * perf]
sum(data_daily_trend_lim$capital, na.rm = T)
clipr::write_clip(data_daily_trend_lim)
