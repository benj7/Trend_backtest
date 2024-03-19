plot_trendfollowing <- function(plot_bollinger = FALSE,
                                plot_ma_ct = FALSE,
                                plot_ma_lt = FALSE,
                                plot_safeline = FALSE,
                                plot_frametrend = FALSE) {
    
    if(nrow(monthly) >= 200) {
        
        cols <- c("date", "open", "adj_close", "low", "high", "mavg", "bol_inf", "bol_sup", 
                  "wma4", "wma12", "ema21", "sma50", "wma150", "sma200")
        
    } else {
        
        cols <- c("date", "open", "adj_close", "low", "high", "mavg", "bol_inf", "bol_sup", 
                  "wma4", "wma12", "ema21", "sma50", "wma150")
    }
    
    safeline <- as.xts(daily[, .(date, safeline, safelinelow)])
    
    frametrend <- as.xts(daily[, .(date, upper, lower)])
    
    data_m <- monthly[, .SD, .SDcols = cols]
    data_m <- data_m[!duplicated(data_m)]
    data_m <- as.xts(data_m)
    
    data_w <- weekly[, .SD, .SDcols = cols]
    data_w <- data_w[!duplicated(data_w)]
    data_w <- as.xts(data_w)
    
    data_d <- daily[, .SD, .SDcols = cols]
    data_d <- data_d[!duplicated(data_d)]
    data_d <- as.xts(data_d)
    
    
    hc <- highchart(type = "stock") |> 
        
        # Create 3 different axis - same height for each 
        hc_yAxis_multiples(create_axis(3, height = c(1, 1, 1), turnopposite = TRUE)) |> 
        
        # Handle candlestick colors 
        hc_plotOptions(
            candlestick = list(
                upColor = "#8cdda4",
                upLineColor = "#8cdda4",
                color =  "#fe2e40",
                lineColor = "#fe2e40")) |> 
        
        # hc_plotOptions(
        #     # avoid hide series due bug
        #     series = list(point = list(events = list(legendItemClick = JS("function(e) {e.preventDefault() }"))))
        # ) |> 
        
        # Monthly data 
        hc_add_series(data_m, yAxis = 0, name = "monthly") |> 
        hc_add_series(data_m$wma4, yAxis = 0, name = "wma4_monthly", color = "pink") |> 
        
        #  Weekly data 
        hc_add_series(data_w, yAxis = 1, name = "weekly") |> 
        hc_add_series(data_w$wma4, yAxis = 1, name = "wma4_weekly", color = "pink") |> 
        
        # daily data 
        hc_add_series(data_d, yAxis = 2, name = "daily" ) |> 
        hc_add_series(data_d$wma4, yAxis = 2, name = "wma4_daily", color = "pink") 
    
    if(plot_bollinger) {
        
        hc <- hc |> 
            hc_add_series(data_m$mavg, yAxis = 0, name = "mavg_monthly", type ="line", dashStyle = "dash", color = "white") |> 
            hc_add_series(data_m$bol_inf, yAxis = 0, name = "bol_inf_monthly", color = "white") |> 
            hc_add_series(data_m$bol_sup, yAxis = 0, name = "bol_sup_monthly", color = "white") |> 
            
            hc_add_series(data_w$mavg, yAxis = 1, name = "mavg_weekly", type ="line", dashStyle = "dash", color = "white") |> 
            hc_add_series(data_w$bol_inf, yAxis = 1, name = "bol_inf_weekly", color = "white") |> 
            hc_add_series(data_w$bol_sup, yAxis = 1, name = "bol_sup_weekly", color = "white") |>
            
            hc_add_series(data_d$mavg, yAxis = 2, name = "mavg_daily", type ="line", dashStyle = "dash", color = "white") |> 
            hc_add_series(data_d$bol_inf, yAxis = 2, name = "bol_inf_daily", color = "white") |> 
            hc_add_series(data_d$bol_sup, yAxis = 2, name = "bol_sup_daily", color = "white") 
    }
    
    
    # Short-term moving averages
    if(plot_ma_ct) {
        
        hc <- hc |>
            
            hc_add_series(data_w$wma12, yAxis = 1, name = "wma12_weekly", color = "#e28743") |>
            hc_add_series(data_w$ema21, yAxis = 1, name = "ema21_weekly", color = "#eab676") |> 
            hc_add_series(data_w$sma50, yAxis = 1, name = "sma50_weekly", color = "#76b5c5") |> 
            hc_add_series(data_w$wma150, yAxis = 1, name = "wma150_weekly", color = "#1e81b0") |> 
            
            hc_add_series(data_d$wma12, yAxis = 2, name = "wma12_daily", color = "#e28743") |>
            hc_add_series(data_d$ema21, yAxis = 2, name = "ema21_daily", color = "#eab676") |> 
            hc_add_series(data_d$sma50, yAxis = 2, name = "sma50_daily", color = "#76b5c5") |> 
            hc_add_series(data_d$wma150, yAxis = 2, name = "wma150_daily", color = "#1e81b0")
        
    }
    
    # Mid-term moving averages
    if(plot_ma_lt & nrow(data_d) >= 200) {
        
        hc <- hc |> 
            
            hc_add_series(data_d$sma200, yAxis = 2, name = "sma200_daily", color = "#961eb0")
    }
    
    if(plot_safeline){
        
        hc <- hc |> 
            # Safeline + Stop loss
            hc_add_series(safeline$safeline, yAxis = 2, name = "safeline", color = "yellow") |>
            hc_add_series(safeline$safelinelow, yAxis = 2, name = "safelinelow", type ="line", dashStyle = "dash", color = "yellow")
        
    }
    
    if(plot_frametrend){
        
        hc <- hc |> 
            # Upper and lower bands to frame the trend 
            hc_add_series(frametrend$upper, yAxis = 2, name = "upper", color = "lightred") |>
            hc_add_series(frametrend$lower, yAxis = 2, name = "lower", color = "lightred") 
        
    }
    
    hc <- hc |> 
        
        hc_title(text = str_c(toupper(asset), "MONTHLY - WEEKLY - DAILY", sep = "<br/>"),
                 margin = 20,
                 align = "left",
                 style = list(color = "#22A884", useHTML = TRUE)) |> 
        
        hc_tooltip(enabled = TRUE,
                   valueDecimals = 2) |> 
        
        hc_legend(enabled = TRUE, 
                  crosshairs = FALSE) |> 
        
        hc_add_theme(hc_theme_alone())
    
    return(hc)
    
}


# Plot for daily data only  -----------------------------------------------

plot_daily <- function(plot_bollinger = FALSE,
                       plot_ma_ct = FALSE,
                       plot_ma_lt = FALSE,
                       plot_safeline = FALSE,
                       plot_frametrend = FALSE,
                       plot_keltner_dn = FALSE,
                       plot_atr = FALSE) {
    
    
    cols <- c("date", "open", "adj_close", "low", "high", "mavg", "bol_inf", "bol_sup", 
              "wma4", "wma12", "ema21", "sma50", "wma150", "sma200", "atr4")
    
    safeline <- as.xts(daily[, .(date, safeline, safelinelow)])
    
    frametrend <- as.xts(daily[, .(date, lower, upper)])
    
    keltner <- as.xts(daily[, .(date, keltner_dn)])
    
    data_d <- daily[, .SD, .SDcols = cols]
    data_d <- data_d[!duplicated(data_d)]
    data_d <- as.xts(data_d)
    
    nb_axes <- ifelse(plot_atr, 2, 1)
    
    height <- if(plot_atr){
        
        c(2, 1) 
        
    } else {
        
        1
    }
    
    hc <- highchart(type = "stock") |> 
        
        # Create 3 different axis - same height for each 
        hc_yAxis_multiples(create_axis(nb_axes, height = height, turnopposite = TRUE)) |> 
        
        # Handle candlestick colors 
        hc_plotOptions(
            candlestick = list(
                upColor = "#8cdda4",
                upLineColor = "#8cdda4",
                color =  "#fe2e40",
                lineColor = "#fe2e40")) |> 
        
        # daily data 
        hc_add_series(data_d, yAxis = 0, name = "daily" ) |> 
        hc_add_series(data_d$wma4, yAxis = 0, name = "wma4_daily", color = "pink") 
    
    if(plot_bollinger) {
        
        hc <- hc |> 
            hc_add_series(data_d$mavg, yAxis = 0, name = "mavg_daily", type ="line", dashStyle = "dash", color = "white") |> 
            hc_add_series(data_d$bol_inf, yAxis = 0, name = "bol_inf_daily", color = "white") |> 
            hc_add_series(data_d$bol_sup, yAxis = 0, name = "bol_sup_daily", color = "white") 
    }
    
    
    # Short-term moving averages
    if(plot_ma_ct) {
        
        hc <- hc |>
            hc_add_series(data_d$wma12, yAxis = 0, name = "wma12_daily", color = "#e28743") |>
            hc_add_series(data_d$ema21, yAxis = 0, name = "ema21_daily", color = "#eab676") |> 
            hc_add_series(data_d$sma50, yAxis = 0, name = "sma50_daily", color = "#76b5c5")
        
    }
    
    # Mid-term moving averages
    if(plot_ma_lt) {
        
        hc <- hc |>
            hc_add_series(data_d$wma150, yAxis = 0, name = "wma150_daily", color = "#1e81b0") |>
            hc_add_series(data_d$sma200, yAxis = 0, name = "sma200_daily", color = "#961eb0")
    }
    
    if(plot_safeline){
        
        hc <- hc |> 
            # Safeline + Stop loss
            hc_add_series(safeline$safeline, yAxis = 0, name = "safeline", color = "deeppink") |>
            hc_add_series(safeline$safelinelow, yAxis = 0, name = "safelinelow", type ="line", dashStyle = "dash", color = "deeppink")
        
    }
    
    if(plot_frametrend){
        
        hc <- hc |> 
            # Upper and lower bands to frame the trend 
            hc_add_series(frametrend$lower, yAxis = 0, name = "lower", color = "grey") |>
            hc_add_series(frametrend$upper, yAxis = 0, name = "upper", color = "grey") 
        
    }
    
    if(plot_keltner_dn){
        
        hc <- hc |> 
            # Upper and lower bands to frame the trend 
            hc_add_series(keltner$keltner_dn, yAxis = 0, name = "keltner_sl", color = "purple")
        
    }
    
    if(plot_atr){
        
        hc <- hc |> 
            # Upper and lower bands to frame the trend 
            hc_add_series(data_d$atr4, yAxis = 1, name = "atr4_days", color = "white")
        
    }
    
    hc <- hc |> 
        
        hc_title(text = str_c(toupper(asset), "DAILY", sep = "<br/>"),
                 margin = 20,
                 align = "left",
                 style = list(color = "#22A884", useHTML = TRUE)) |> 
        
        hc_tooltip(enabled = TRUE,
                   valueDecimals = 2) |> 
        
        hc_legend(enabled = TRUE, 
                  crosshairs = FALSE) |> 
        
        hc_add_theme(hc_theme_alone())
    
    return(hc)
    
}

# Plot distance between adj close and bol upper ---------------------------
cac_daily <- read_rds(str_c("output/data/cac_daily_modified.rds"))
cac_weekly <- read_rds(str_c("output/data/cac_weekly_modified.rds"))
cac_monthly <- read_rds(str_c("output/data/cac_monthly_modified.rds"))

plot_distance_bol_sup <- function(index = FALSE) {
    
    cols <- c("date", "distance_bol_sup")
    
    if(index){
        
        monthly <- cac_monthly
        weekly <- cac_weekly
        daily <- cac_daily
        
    }
    
    data_m <- monthly[, .SD, .SDcols = cols]
    data_m <- data_m[!duplicated(data_m)]
    data_m <- as.xts(data_m)
    
    data_w <- weekly[, .SD, .SDcols = cols]
    data_w <- data_w[!duplicated(data_w)]
    data_w <- as.xts(data_w)
    
    data_d <- daily[, .SD, .SDcols = cols]
    data_d <- data_d[!duplicated(data_d)]
    data_d <- as.xts(data_d)
    
    
    hc <- highchart(type = "stock") |> 
        
        # Create 1  axis 
        hc_yAxis_multiples(create_axis(1, height = 1, turnopposite = TRUE)) |> 
        
        # Handle candlestick colors 
        hc_plotOptions(
            candlestick = list(
                upColor = "#8cdda4",
                upLineColor = "#8cdda4",
                color =  "#fe2e40",
                lineColor = "#fe2e40")) |> 
        
        # Monthly data
        hc_add_series(data_m$distance_bol_sup, yAxis = 0, name = "distance_bol_sup_monthly", color = "lightpink") |> 
        
        #  Weekly data 
        hc_add_series(data_w$distance_bol_sup, yAxis = 0, name = "distance_bol_sup_weekly", color = "lightgreen") |> 
        
        # daily data 
        hc_add_series(data_d$distance_bol_sup, yAxis = 0, name = "distance_bol_sup_daily", color = "lightblue") 
    
    hc <- hc |> 
        
        hc_title(text = str_c(toupper(ifelse(index, "CAC", asset)), "Distance between Close and Upper bollinger band", sep = "<br/>"),
                 margin = 20,
                 align = "left",
                 style = list(color = "#22A884", useHTML = TRUE)) |> 
        
        hc_tooltip(enabled = TRUE,
                   valueDecimals = 2) |> 
        
        hc_legend(enabled = TRUE, 
                  crosshairs = FALSE) |> 
        
        hc_add_theme(hc_theme_alone())
    
    return(hc)
    
}

plot_distance_bol_sup(index = FALSE)

# Plot CAC40 index against one specific Stock price

plot_cac_stock <- function(){
    
    cols <- c("date", "adj_close")
    
    start_date <- max(min(cac_daily$date), 
                      min(daily$date))
    
    end_date <- min(max(cac_daily$date), 
                    max(daily$date))
    
    # Index prices
    cac_m <- cac_monthly[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_m <- cac_m[date == first(date), adj_close]/100
    cac_m <- cac_m[, adj_close := adj_close * 1/factor_100_m]
    cac_m <- cac_m[!duplicated(cac_m)]
    cac_m <- as.xts(cac_m)
    
    cac_w <- cac_weekly[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_w <- cac_w[date == first(date), adj_close]/100
    cac_w <- cac_w[, adj_close := adj_close * 1/factor_100_w]
    cac_w <- cac_w[!duplicated(cac_w)]
    cac_w <- as.xts(cac_w)
    
    cac_d <- cac_daily[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_d <- cac_d[date == first(date), adj_close]/100
    cac_d <- cac_d[, adj_close := adj_close * 1/factor_100_d]
    cac_d <- cac_d[!duplicated(cac_d)]
    cac_d <- as.xts(cac_d)
    
    # Stock prices
    data_m <- monthly[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_m <- data_m[date == first(date), adj_close]/100
    data_m <- data_m[, adj_close := adj_close * 1/factor_100_m]
    data_m <- data_m[!duplicated(data_m)]
    data_m <- as.xts(data_m)
    
    data_w <- weekly[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_w <- data_w[date == first(date), adj_close]/100
    data_w <- data_w[, adj_close := adj_close * 1/factor_100_w]
    data_w <- data_w[!duplicated(data_w)]
    data_w <- as.xts(data_w)
    
    data_d <- daily[between(date, start_date, end_date), .SD, .SDcols = cols]
    factor_100_d <- data_d[date == first(date), adj_close]/100
    data_d <- data_d[, adj_close := adj_close * 1/factor_100_d]
    data_d <- data_d[!duplicated(data_d)]
    data_d <- as.xts(data_d)
    
    hc <- highchart(type = "stock") |> 
        
        # Create 1  axis 
        hc_yAxis_multiples(create_axis(1, height = 1, turnopposite = TRUE)) |> 
        
        # Handle candlestick colors 
        hc_plotOptions(
            candlestick = list(
                upColor = "#8cdda4",
                upLineColor = "#8cdda4",
                color =  "#fe2e40",
                lineColor = "#fe2e40")) |> 
        
        # Monthly data
        hc_add_series(cac_m$adj_close, yAxis = 0, name = "cac_adj_close_monthly", color = "pink") |> 
        hc_add_series(data_m$adj_close, yAxis = 0, name = "adj_close_monthly", color = "deeppink") |> 
        
        #  Weekly data 
        hc_add_series(cac_w$adj_close, yAxis = 0, name = "cac_adj_close_weekly", color = "lightgreen") |> 
        hc_add_series(data_w$adj_close, yAxis = 0, name = "adj_close_weekly", color = "green") |> 
        
        # daily data 
        hc_add_series(cac_d$adj_close, yAxis = 0, name = "cac_adj_close_daily", color = "lightblue") |> 
        hc_add_series(data_d$adj_close, yAxis = 0, name = "adj_close_daily", color = "blue")
    
    hc <- hc |> 
        
        hc_title(text = "CAC Index against Stock price",
                 margin = 20,
                 align = "left",
                 style = list(color = "#22A884", useHTML = TRUE)) |> 
        
        hc_tooltip(enabled = TRUE,
                   valueDecimals = 2) |> 
        
        hc_legend(enabled = TRUE, 
                  crosshairs = FALSE) |> 
        
        hc_add_theme(hc_theme_alone())
    
    return(hc)
    
}

plot_cac_stock()
