implement_strategy_trend_mensuel <- function(data,
                                             date_debut_strat = NULL,
                                             date_fin_strat = NULL,
                                             signal_entree, 
                                             signal_sortie,
                                             k_init = 1000,
                                             nom_strat = "strategie"){
    
    dt <- copy(data)
    
    if(missing(date_debut_strat)){
        
        date_debut_strat <- min(dt$date)
        
    } else {
        
        date_debut_strat <- date_debut_strat
    }
    
    if(missing(date_fin_strat)){
        
        date_fin_strat <- max(dt$date)
        
    } else {
        
        date_fin_strat <- date_fin_strat
    }
    
    dt <- dt[between(date, date_debut_strat, date_fin_strat)]
    
    dt <- dt[eval(parse(text = signal_entree)), entree_position:= "entree"]
    
    dt <- dt[eval(parse(text = signal_sortie)), sortie_position := "sortie"]
    
    row_entree <- data.table(row_number = dt[entree_position == "entree", which = TRUE], entree_sortie = "entree")
    
    row_sortie <- data.table(row_number = dt[sortie_position == "sortie", which = TRUE], entree_sortie = "sortie")
    
    row_entree_sortie <- rbind(row_entree, row_sortie)
    
    row_entree_sortie <- row_entree_sortie[order(row_number)]
    
    row_entree_sortie <- row_entree_sortie[, prev_entree_sortie := data.table::shift(entree_sortie, n = 1)]
    
    row_entree_sortie <- row_entree_sortie[entree_sortie == "entree" & 
                                               (is.na(prev_entree_sortie) | prev_entree_sortie == "sortie"), signal := "entree"]
    
    row_entree_sortie <- row_entree_sortie[entree_sortie == "sortie" & 
                                               prev_entree_sortie == "entree", signal := "sortie"]
    
    row_entree_sortie <- row_entree_sortie[signal %in% c("entree", "sortie")]
    
    index_entree <- row_entree_sortie[signal == "entree", row_number]
    
    index_sortie <- row_entree_sortie[signal == "sortie", row_number]
    
    tmp1 <- dt[index_entree][, signal := "entree"]
    
    tmp2 <- dt[index_sortie][, signal := "sortie"]
    
    res <- rbind(tmp1, tmp2)
    
    res <- res[order(date)]
    
    if(res[, last(signal)] == "entree"){
        
        res <- head(res, -1)
    } 
    
    res <- res[, prev_close := data.table::shift(adj_close, n = 1)]
    
    res <- res[signal == "entree", prix_entree := adj_close]
    
    res <- res[signal == "sortie", prix_sortie := sma20]
    
    res <- res[signal == "sortie", perf := prix_sortie/prev_close- 1]
    
    vector_perf <- 1+res[!is.na(perf), perf]
    
    perf_cumul <- prod(vector_perf)
    
    k_final <- k_init * perf_cumul
    
    summary <- data.table(strategie = nom_strat,
                          actif = asset, 
                          date_debut_strategie = min(res$date),
                          date_fin_strategie = max(res$date),
                          prix_debut_strategie = res[, first(adj_close)],
                          prix_fin_strategie = res[, last(adj_close)],
                          perf_buy_hold = res[, last(adj_close)]/res[, first(adj_close)],
                          perf_cumul = perf_cumul,
                          capital_initial = k_init,
                          capital_final = k_final,
                          periode_couverte = as.numeric(difftime(max(res$date), min(res$date), units = "days"))/365.25)
    
    summary <- summary[, perf_annualisee_pct := ((perf_cumul)^(1/periode_couverte)-1) * 100]
    
    max_drawdown_pct<- (min(vector_perf) - 1) * 100
    max_perf_pct <- (max(vector_perf) - 1) * 100
    mean_perf_pct <- (mean(vector_perf) - 1) * 100
    
    summary <- summary[, c("max_drawdown_pct", "max_perf_pct", "mean_perf_pct")  := list(max_drawdown_pct,
                                                                                         max_perf_pct,
                                                                                         mean_perf_pct)]
    
    cols <- c("date"
              , "vitesse"
              , "score"
              , "signal"
              , "prix_entree"
              , "prix_sortie"
              , "adj_close"
              , "sma20"
              , "perf")
    
    
    return(list(res[, .SD, .SDcols = cols]
                # , vector_perf
                # , summary
                )
           )
    
}

signal_entree <- "score > 5000"
signal_sortie <- "below_sma20 == 1"

setnames(axa_trend_mensuel, old = "date_analyse", new = "date")

implement_strategy_trend_mensuel(axa_trend_mensuel, 
                                 signal_entree = signal_entree,
                                 signal_sortie = signal_sortie)
