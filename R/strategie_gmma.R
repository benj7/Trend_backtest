strategie_gmma <- function(data_daily,
                           k_init = 1000){
    
    dt_daily <- copy(data_daily)
    
    # Mensuel et hebdo ok comme conditions d'entrée 
    
    dt_daily <- dt_daily[short_term_above_long_term == 1 & ema3 > ema5, entree_position_mm := "entree"]
    
    dt_daily <- dt_daily[ema3 <= ema5 , sortie_position_mm := "sortie"]
    
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

    res <- res[, prev_close := data.table::shift(close, n = 1)]

    res <- res[signal == "sortie", perf := close/prev_close - 1]

    vector_perf <- 1+res[!is.na(perf), perf]

    perf_cumul <- prod(vector_perf)

    k_final <- k_init * perf_cumul

    summary <- data.table(date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(close)],
                          prix_debut_strategie = res[, last(close)],
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

tmp2 <- daily[, .(date, close, short_term_above_long_term)]
strategie_gmma(daily)
res_tmp <- strategie_gmma(daily)[[2]]

res_tmp <- res_tmp[, .(date, close, short_term_above_long_term, signal, perf)][between(date, as.Date("2015-07-01"), as.Date("2015-08-30"))]
