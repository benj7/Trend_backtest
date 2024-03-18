


date_analyse <- as.Date("2008-01-01")
test <- amsterdam_score(data = data_daily,
                        date_analyse = date_analyse,
                        avg_volume_crit = 200000,
                        max_volume_crit = 500000)

test


tmp <- amsterdam_noeud_mm(data = data_daily,
                          date_analyse = "2003-08-30")


dates_analyse <- data_daily[Date >= min(Date) + 100, Date]


plan(multisession, workers = 8)
tic()
res_amsterdam_noeud_mm <- future_map_dfr(as.list(dates_analyse),
                                         amsterdam_noeud_mm)
toc()

res_final_amsterdam_noeud_mm <- res_amsterdam_noeud_mm[, c("vitesse", "date_analyse") := list(NULL, NULL)]
res_final_amsterdam_noeud_mm <- unique(res_final_amsterdam_noeud_mm)
