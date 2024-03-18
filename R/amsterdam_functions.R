# PRT screener 
# vitesse=highest[100](roc[3])
# 
# tmp1=max(average[200],weightedaverage[150])
# tmp2=max(average[50],exponentialaverage[21])
# tmp3=max(tmp1,tmp2)
# allmas=max(tmp3,weightedaverage[12])
# 
# opa=highest[5](high)/lowest[5](low)<1.03
# 
# okvol=average[20](close*volume)>200000 or highest[20](close*volume)>500000
# 
# ok=close>=allmas-averagetruerange[50] and vitesse>=15 and okvol and not opa
# 
# if close>allmas then
# vitesse=vitesse+1000
# if close[1]<allmas[1] then
# vitesse=vitesse+2000
# endif
# endif
# 
# SCREENER[ok](round(vitesse) as "Raid")


# Amsterdam score ----
amsterdam_score <- function(data,
                            date_analyse,
                            avg_volume_crit = 200000,
                            max_volume_crit = 500000,
                            vitesse_crit = 15){
    
    dt <- copy(data)
    
    date_analyse <- as.Date(date_analyse)
    
    start_horizon <- all_max_perf_n_days_horizon[end_horizon == date_analyse, start_horizon]
    
    #  Vitesse - plus grosse variation en 5 jours sur une période de 100 jours
    vitesse_max <- all_max_perf_n_days_horizon[end_horizon == date_analyse, max_perf_n_days]
    
    dt <- dt[between(date, start_horizon, date_analyse)]
    
    dt <- dt[, vitesse := round(vitesse_max*100)]
    
    # Trier sur critère de Volume moyen et Volume max
    dt <- dt[, ok_vol_amsterdam := fifelse(rollmean_close_volume > avg_volume_crit | rollmax_close_volume > max_volume_crit, 1, 0)]
    
    dt <- dt[, ok_amsterdam := fifelse((close >= max_ma - atr_50) &
                                           vitesse >= vitesse_crit &
                                           ok_vol_amsterdam == 1 &
                                           opa == 0,
                                       1,
                                       0)]
    
    # Score < 1000 points - Une valeur va être proche d'être au-dessus de toutes ses moyennes mobiles
    # Score > 1000 points - Au dessus de ses moyennes mobiles
    # Vitesse = plus grosse variation en 5 jours sur les 100 derniers jours
    
    dt <- dt[ok_amsterdam == 1, score_amsterdam := fifelse(close > max_ma, vitesse + 1000, vitesse)]
    
    first_day_above_ma <- if(nrow(dt[ok_amsterdam == 1 & close > max_ma] > 0)) (dt[ok_amsterdam == 1 & close > max_ma, min(date)])
    
    # # Et si c'est le premier jour, elle va avoir un score de 2000 de plus
    # Problème dans le code - car le 1er jour est considéré après filtre sur la période considérée - retranscrit mal tout l'historique donc !! 
    if(!is.null(first_day_above_ma)){
        
        dt <- dt[date == first_day_above_ma, score_amsterdam :=  score_amsterdam + 2000]
        
    }
    
    cols <- c("date"
              , "close"
              , "vitesse"
              , "volume"
              , "max_ma"
              , "atr_50"
              , "opa_high"
              , "opa_low"
              , "opa"
              , "rollmean_close_volume"
              , "rollmax_close_volume"
              , "ok_vol_amsterdam"
              , "ok_amsterdam"
              , "score_amsterdam")
    
    res <- dt[ ,.SD, .SDcols = cols]
    
    return(res)
    
}
date_analyse <- as.Date("2023-07-10")
tmp2 <- amsterdam_score(daily, 
                        date_analyse,
                        vitesse_crit = 5)

# AMSTERDAM RELANCE DE PUISSANCE -------------------------------
# tmp1=max(weightedaverage[12],exponentialaverage[21])
# tmp2=max(tmp1,average[50])
# tmp3=min(weightedaverage[12],exponentialaverage[21])
# tmp4=min(tmp3,average[50])
# 
# vitesse=round(highest[100](roc[5]))
# 
# ok=close>=tmp2-averagetruerange[20] and close>=average[200] and tmp2-tmp4<1*averagetruerange[20] and close*average[20](volume)>200000 and average[200]>average[200][5]
# 
# SCREENER[ok](vitesse as "vitesse")

# Amsterdam Relance de puissance

amsterdam_relance_puissance <- function(data,
                                        date_analyse = NULL,
                                        avg_volume_crit = 200000,
                                        max_volume_crit = 500000){
    
    dt <- copy(data)
    
    if(missing(date_analyse)) {
        
        date_analyse <- max(daily$date)
        
    } else {
        
        date_analyse <- as.Date(date_analyse)
    }
    
    # start_horizon <- all_max_perf_n_days_horizon[end_horizon == date_analyse, start_horizon]
    # 
    # #  Vitesse - plus grosse variation en 5 jours sur une période de 100 jours
    # vitesse_max <- all_max_perf_n_days_horizon[end_horizon == date_analyse, max_perf_n_days]
    # 
    # dt <- dt[between(date, start_horizon, date_analyse)]
    # 
    # dt <- dt[, vitesse := round(vitesse_max*100)]
    
    # Trier sur critère de Volume moyen et Volume max
    
    dt <- dt[, ok_vol_amsterdam := fifelse(rollmean_close_volume > avg_volume_crit, 1, 0)]
    
    dt <- dt[, ok_relance := fifelse(ok_vol_amsterdam == 1 &
                                         noeud_mm_ct == 1 &
                                         # noeud_mm_mt == 1 &
                                         opa == 0 &
                                         above_sma200 == 1 &
                                         sma200_up == 1,
                                     1,
                                     0)]
    
    # cols <- c("date"
    #           , "open"
    #           , "adj_close"
    #           , "low"
    #           , "taille_bougie"
    #           , "mavg_taille_bougie"
    #           , "median_taille_bougie"
    #           , "sma200"
    #           , "above_sma200"
    #           , "sma200_up"
    #           , "wma4"
    #           , "wma4_up"
    #           , "wma12"
    #           , "ema21"
    #           , "flux_haussier"
    #           , "sma50"
    #           , "below_sma50"
    #           , "atr_20"
    #           , "noeud_mm_ct"
    #           , "petite_bougie"
    #           , "petite_bougie_prev"
    #           , "petite_bougie_ante_prev"
    #           , "safeline"
    #           , "safelinelow"
    #           , "above_safeline"
    #           , "below_safelinelow"
    #           # , "vitesse"
    #           , "volume"
    #           , "opa_high"
    #           , "opa_low"
    #           , "opa"
    #           , "rollmean_close_volume"
    #           , "ok_vol_amsterdam"
    #           , "ok_relance")
    
    # dt <- dt[, .SD, .SDcols = cols]
    
    return(dt)
    
}

daily <- amsterdam_relance_puissance(daily)

# AMSTERDAM NOEUD MM ------------------------------------------------------

# vitesse=highest[100](roc[20])
# 
# tmp1=max(weightedaverage[12],exponentialaverage[21])
# tmp2=min(weightedaverage[12],exponentialaverage[21])
# tmp3=max(tmp1,close)
# tmp4=min(tmp2,close)
# tmp5=max(tmp3,tmp4)-min(tmp3,tmp4)
# 
# opa=highest[5](high)/lowest[5](low)<1.03
# 
# okvol=average[20](close*volume)>200000 or highest[20](close*volume)>500000
# 
# ok=tmp5<0.5*averagetruerange[20] and close>average[200]
# 
# score=vitesse
# 
# SCREENER[ok and okvol and not opa](round(score) as "Score")

amsterdam_noeud_mm <- function(data,
                               date_analyse = NULL,
                               avg_volume_crit = 200000,
                               max_volume_crit = 500000){
    
    if(missing(date_analyse)) {
        
        date_analyse <- max(daily$date)
        
    } else {
        
        date_analyse <- as.Date(date_analyse)
    }
    
    # start_horizon <- all_max_perf_n_days_horizon[end_horizon == date_analyse, start_horizon]
    # 
    # #  Vitesse - plus grosse variation en 5 jours sur une période de 100 jours
    # vitesse_max <- all_max_perf_n_days_horizon[end_horizon == date_analyse, max_perf_n_days]
    # 
    # dt <- dt[between(date, start_horizon, date_analyse)]
    # 
    # dt <- dt[, vitesse := round(vitesse_max*100)]
    
    # Trier sur critère de Volume moyen et Volume max
    dt <- dt[, ok_vol_amsterdam := fifelse((rollmean_close_volume > avg_volume_crit | rollmax_close_volume > max_volume_crit), 1, 0)]
    
    # dt <- dt[, ok_noeud_mm := fifelse(ok_vol_amsterdam == 1 &
    #                                       noeud_mm_ct == 1 &
    #                                       opa == 0 &
    #                                       above_sma200 == 1 &
    #                                       sma200_up == 1,
    #                                   1,
    #                                   0)]
    # 
    # cols <- c("date"
    #           , "adj_close"
    #           , "low"
    #           , "sma200"
    #           , "above_sma200"
    #           , "sma200_up"
    #           , "wma4"
    #           , "wma4_up"
    #           , "wma12"
    #           , "ema21"
    #           , "sma50"
    #           , "atr_20"
    #           , "noeud_mm_ct"
    #           , "petite_bougie"
    #           , "petite_bougie_prev"
    #           , "petite_bougie_ante_prev"
    #           , "safelinelow"
    #           , "above_safeline"
    #           , "below_safelinelow"
    #           # , "vitesse"
    #           , "volume"
    #           , "opa_high"
    #           , "opa_low"
    #           , "opa"
    #           , "rollmean_close_volume"
    #           , "ok_vol_amsterdam"
    #           , "ok_noeud_mm")
    # 
    # dt <- dt[, .SD, .SDcols = cols]
    
    return(dt)
    
}

date_analyse <- as.Date("2023-07-10")
amsterdam_noeud_mm(daily)

# dates_analyse <- data_daily[date >= min(date) + 100, date]
# dates_analyse_part1 <- dates_analyse[1:3000]
# dates_analyse_part2 <- dates_analyse[3001:length(dates_analyse)]
# 
# len <- length(dates_analyse_part1)
# 
# list_dt <- replicate(n = len,
#                      expr = {data_daily},
#                      simplify = F)
# 
# plan(multisession, workers = 8)
# tic()
# res_amsterdam_noeud_mm <- future_map_dfr(list(list_dt,
#                                               as.list(dates_analyse_part1)),
#                                          amsterdam_noeud_mm)
# toc()
# 
# amsterdam_noeud_mm(data = apple_daily_daily,
#                    date_analyse = "2012-05-10",
#                    avg_volume_crit = 200000,
#                    max_volume_crit = 500000)




