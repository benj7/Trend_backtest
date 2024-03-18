# Score Montreux Trend Following 

montreux_score <- function(data_daily,
                           data_weekly,
                           data_monthly,
                           date_analyse = NULL){
    
    dt_daily <- copy(data_daily)
    dt_weekly <- copy(data_weekly)
    dt_monthly <- copy(data_monthly)
    
    if(missing(date_analyse)){
        
        date_analyse <- max(dt_daily$date)
    }
    
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
    
    dt_daily <- dt_daily[, ok_vol_opa := fifelse(ok_vol == 1 &
                                                     opa == 0,
                                                  1,
                                                  0)]
    
    # Score < 1000 points - Une valeur va être proche d'être au-dessus de toutes ses moyennes mobiles
    # Score > 1000 points - Au dessus de ses moyennes mobiles
    # Vitesse = plus grosse variation en 3 jours sur les 100 derniers jours
    
    # Daily OK
    dt_daily <- dt_daily[, score_montreux := fifelse(above_bol_sup == 1, vitesse + 1000, vitesse)]
    
    dt_daily <- dt_daily[close > bol_sup, ok_daily := 1]
    
    # weeks_year_montreux_daily <- dt_daily[ok_montreux == 1, unique(week_year)]
    
    # weeks_year_montreux_daily_weekly <- dt_weekly[week_year %in% weeks_year_montreux_daily & above_bol_sup == 1,  unique(week_year)]
    
    # months_year_montreux_daily_weekly <- dt_monthly[week_year %in% weeks_year_montreux_daily & above_bol_sup == 1,  unique(month_year)]
    
    # Weekly OK 
    # dt_daily <- dt_daily[week_year %in% weeks_year_montreux_daily_weekly, score_montreux := score_montreux + 2000]
    
    # Monthly OK 
    # dt_daily <- dt_daily[month_year %in% months_year_montreux_daily_weekly, score_montreux := score_montreux + 4000]
    
    # Weekly OK 
    dt_daily <- dt_daily[close > bol_sup_weekly, c("ok_weekly", "score_montreux") := list(1,  score_montreux + 2000) ]
    
    # Monthly OK 
    dt_daily <- dt_daily[close > bol_sup_monthly, c("ok_monthly", "score_montreux") := list(1,  score_montreux + 4000) ]
    
    # ok_vol_opa == 1 & ok_monthly == 1 & vitesse >= 15 & score_montreux >= 2000
    
    res_daily <- dt_daily[,
                          .(date, week_year, close, bol_sup, above_bol_sup, bol_sup_weekly, bol_sup_monthly, opa, ok_vol_opa, vitesse, score_montreux)]
    
    return(res_daily)
    
    
}


# testing zone ------------------------------------------------------------

montreux_score(daily, 
               weekly,
               monthly,
               date_analyse = "2016-12-01")
