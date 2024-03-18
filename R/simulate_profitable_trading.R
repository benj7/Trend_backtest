# Simulation système profitable avec faible taux de réussite 33% mais 3% de gain et 1% de perte 

options(scipen = 999)

profitable_system <- function(win_rate, 
                              reward = NULL, 
                              risk = NULL,
                              n,
                              initial_capital = 1000){
    
    loss_trades <- rep(0, round(n * (1-win_rate)))
    
    win_trades <- rep(1, round(n * win_rate))
    
    trades <- sample(c(loss_trades, win_trades), replace = FALSE)
    
    res <- data.table(trades = trades) 
    
    res <- res[, perf := fifelse(trades == 0, 1+risk, 1+reward)]
    
    res <- res[, cum_perf := cumprod(perf)]
    
    res <- res[, capital := initial_capital * cum_perf]
    
    res <- data.table(win_rate = win_rate, 
                      reward = reward, 
                      risk = risk, 
                      nb_trades = n,
                      initial_capital = initial_capital, 
                      last_capital = res[, last(capital)],
                      perf_pct = scales::percent(res[, last(capital)]/initial_capital - 1))
    
    return(res)
    
}

tmp2 <- profitable_system(win_rate = 1/3,
                          reward = 3/100,
                          risk = -1/100,
                          n = 1000,
                          initial_capital = 1000)
tmp2[]

