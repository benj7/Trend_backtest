# Principe général : poussée/consolidation/résistance/cassure/redémarrage

# 1. Détecter les actions ayant le plus monté sur 5 jours considérant un horizon de 100 jours à partir d'une date d'analyse 

ndays <- 5

res_horizon_n_days <- get_start_end_periods(daily, 
                                            horizon = 100,
                                            ndays = ndays)

all_max_perf_n_days_horizon <- pmap_dfr(list(res_horizon_n_days$start_horizon,
                                             res_horizon_n_days$end_date),
                                        max_perf_horizon)

all_max_perf_n_days_horizon <- all_max_perf_n_days_horizon[order(-max_perf_5_days)]


# 2. Détecter une consolidation à travers 
# compression bandes de bollinger ou Noeud de moyennes mobiles amsterdam relance de puissance 

debut_dates_obs <- all_max_perf_n_days_horizon[, .(debut_obs = min(end_horizon)), by = .(max_perf_5_days)]

debut_obs <- debut_dates_obs$debut_obs

daily[ok_relance == 1 & between(date, debut_obs[6], debut_obs[6] + 100)]

