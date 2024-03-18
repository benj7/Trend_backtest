# Market timing on CAC40 indice 

cac_daily <- read_rds(str_c("output/data/cac_daily_modified.rds"))
cac_weekly <- read_rds(str_c("output/data/cac_weekly_modified.rds"))
cac_monthly <- read_rds(str_c("output/data/cac_monthly_modified.rds"))

cac_bullish <- function(data){
    
    dt <- copy(data)
    
    dt <- dt[!is.na(flux_haussier), .(date, flux_haussier, adj_close)]
    
    dt <- dt[, flux_haussier_prev := data.table::shift(flux_haussier, n = 1)]
    
    dt <- dt[flux_haussier == 1 & 
                 (is.na(flux_haussier_prev) | flux_haussier_prev == 0), signal := "entree_bullish"]
    
    dt <- dt[flux_haussier == 0 & flux_haussier_prev == 1, signal := "sortie_bullish"]
    
    dt <- dt[signal %in% c("entree_bullish", "sortie_bullish"), .(date, flux_haussier, signal, adj_close)]
    
    # dt <- dcast(dt, date ~ signal, value.var = "date")
    
    # dt <- dt[, prev_close := data.table::shift(adj_close, n = 1)]
    # 
    # dt <- dt[signal == "sortie_bullish", perf := adj_close/prev_close]
    # 
    # perf <- dt[!is.na(perf), cumprod(1+perf)]
    
    return(dt)
    
}

cac_bullish_res <- cac_bullish(cac_daily)

test <- cac_bullish_res[[1]]

# Ajouter code pour calculer distance entre close et wma12 pour s'assurer que les cours ne sont pas trop étendus 
# Prise de profit partiel : stop-win fragmentés 
# Ajouter code pour repérer compression de volatilité 

