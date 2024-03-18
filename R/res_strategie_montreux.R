# CAC40 
start_date <- as.Date("2000-01-01")
daily_f <- daily[date >= start_date]
weekly_f <- weekly[date >= start_date]
monthly_f <- monthly[date >= start_date]

tmp <-  montreux_score(daily_f,
                       weekly_f,
                       monthly_f
                       # date_analyse = "2020-11-30",
                       # vitesse_crit = 7
)

daily_f <- tmp[[1]]

tmppp <- strategie_montreux(data_daily = daily_f,
                            data_monthly = monthly_f)

tmppp
tmp <- tmppp[[1]]
prod(tmppp[[3]])
test <- tmppp[[3]]
test <- test[, .(date, close, signal, perf)]
tmppp[[4]]

test[,.(date,signal,score_montreux,close,bol_sup,above_bol_sup,wma4, perf)]
tmp2[!is.na(perf)][order(perf)]

# only keep the important variables of daily data 

daily_lim <- daily[!is.na(wma4_up_monthly), .(date, ok_montreux, score_montreux, 
                                              rollmean_close_volume,
                                              ok_vol, opa, close, bol_sup,
                                              above_bol_sup, bol_sup_weekly,
                                              bol_sup_monthly, wma4_monthly, wma4_up_monthly)]



tmppp <- strat_sma(data_daily = daily_f)

tmp <- tmppp[[1]]


sl_reached <- function(date_entree,
                       date_sortie){
    
    dt <- copy(daily)
    
    dt <- dt[between(date, date_entree, date_sortie)]
    
    dt <- dt[low == min(low), .(date, low)]
    
    dt <- dt[date == min(date)]
    
    return(dt)
}


tmp2 <- purrr::pmap_dfr(list(tmp$date_entree + 1,
                             tmp$date_sortie),
                        sl_reached)

tmp <- cbind(tmp,
             tmp2)

tmp <- tmp[, sl_reached := fifelse(low < sl, 1, 0)]

tmp <- tmp[, perf := prix_sortie / prix_entree -1]

tmp <- tmp[sl_reached == 1, perf := sl / prix_entree -1]

prod(1+tmp$perf)*100

tmppp <- strategie_amsterdam(data = daily_f)
tmp <- tmppp[[1]][, .(date, close, signal, perf)]
max(tmp$perf, na.rm = T)
