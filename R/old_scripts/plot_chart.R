
start_date <- as.Date("2003-05-01")
horizon_days <- 500
end_date <- start_date + horizon_days

# Input dates specified 
generate_plot(start_date = start_date,
              end_date = end_date)

# No input dates needed 
generate_plot()


generate_plot(start_date = start_date)


plot_candlestick(daily, 
                 start_date = start_date,
                 end_date = end_date)



add_buttons_zoom <- function(echart) {
    
    data <- copy(daily)
    
    min_date <- min(data$date)
    max_date <- max(data$date)
    
    oneyear <- max_date - 365
    sixmonths <- max_date - 182
    threemonths <- max_date - 92
    onemonth <- max_date - 30
    ytd <- as.Date(str_c(year(max_date), 1, 1, sep = "-")) 
    
    p_echart <- echart |> 
        
        e_datazoom() |>
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = onemonth,
            endValue = max_date,
            btn = "btn_1m" ) |>
        e_button("btn_1m", "1m") |> 
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = threemonths,
            endValue = max_date,
            btn = "btn_3m" ) |>
        e_button("btn_3m", "3m") |> 
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = sixmonths,
            endValue = max_date,
            btn = "btn_6m" ) |>
        e_button("btn_6m", "6m") |> 
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = ytd,
            endValue = max_date,
            btn = "btn_ytd" ) |>
        e_button("btn_ytd", "YTD") |> 
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = oneyear,
            endValue = max_date,
            btn = "btn_1y") |>
        e_button("btn_1y", "1y") |> 
        
        e_zoom(
            dataZoomIndex = 0,
            startValue = min_date,
            endValue = max_date,
            btn = "btn_all") |>
        e_button("btn_all", "All") 
    
    return(p_echart)
    
}

library(RColorBrewer)


trend_chart  <- function(data,
                         elementId, 
                         show = NULL,
                         title = NULL,
                         subtitle = NULL) {
    
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
 
    plot <- data |> 
        
        e_charts(date,
                 elementId = elementId) |>
        
        e_candle( opening = close, closing = open, low = low, high = high, name = input, 
                  itemStyle = list(
                      color0 = "#e64146", # bear
                      color = "#a8cfb4", # bull
                      borderColor = NA,
                      borderColor0 = NA))|> 
        
        e_line(bol_inf, showSymbol = FALSE) |> 
        e_line(bol_sup,  showSymbol = FALSE) |> 
        e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
        
        e_line(wma4, showSymbol = FALSE) |> 
        e_line(wma12, showSymbol = FALSE) |> 
        e_line(ema21, showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
        e_line(sma50, showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
        
        e_line(keltner_dn, showSymbol = FALSE) |> 
        # e_line(sma200,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
        
        e_color(c(c("white", "white", "white"),
                  sample(col_vector, 1),
                  sample(col_vector, 1),
                  sample(col_vector, 1),
                  sample(col_vector, 1),
                  sample(col_vector, 1),
                  sample(col_vector, 1))) |> 
        
        e_datazoom(show = TRUE) |> 
        e_title(title, subtitle) |> 
        e_theme("dark") |> 
        e_tooltip(trigger = "axis") 
    
    return(plot)
    
}

data_daily_chart <- daily[, .(date = as.character(date), open, close, low, high, bol_inf, bol_sup, mavg, wma4, wma12, ema21, sma50, keltner_dn)] 
data_daily_chart <- data_daily_chart[!duplicated(data_daily_chart)]

data_weekly_chart <- weekly[, .(date = as.character(date), open, close, low, high, bol_inf, bol_sup, mavg, wma4, wma12, ema21, sma50, keltner_dn)] 
data_weekly_chart <- data_weekly_chart[!duplicated(data_weekly_chart)]

data_monthly_chart <- monthly[, .(date = as.character(date), open, close, low, high, bol_inf, bol_sup, mavg, wma4, wma12, ema21, sma50, keltner_dn)] 
data_monthly_chart <- data_monthly_chart[!duplicated(data_monthly_chart)]

monthly_chart <- trend_chart(data_monthly_chart, 
                             elementId = "monthly_chart",
                             show = FALSE,
                             title = "MONTHLY",
                             subtitle = "OpenBB Data") |> 
    add_buttons_zoom()

weekly_chart <- trend_chart(data_weekly_chart, 
                            elementId = "weekly_chart",
                            title = "WEEKLY",
                            show = FALSE) |> 
    add_buttons_zoom()

daily_chart <- trend_chart(data_daily_chart, 
                           elementId = "daily_chart",
                           title = "DAILY",
                           show = FALSE) |> 
    add_buttons_zoom() |> 
    e_connect(c("monthly_chart", "weekly_chart")) 

e_arrange(monthly_chart,
          weekly_chart,
          daily_chart)



plot_d <- tmp |> 
    e_charts() |> 
    e_candle(opening = open, closing = close, low = low, high = high, name = input)|> 
    e_line(bol_inf, showSymbol = FALSE) |> 
    e_line(bol_sup,  showSymbol = FALSE) |> 
    e_line(mavg,  showSymbol = FALSE, lineStyle = list(type = "dotted")) |> 
    e_line(wma12,  showSymbol = FALSE) |> 
    e_line(ema21,  showSymbol = FALSE) |> 
    e_line(sma50,  showSymbol = FALSE) |> 
    e_line(sma200,  showSymbol = FALSE) |> 
    e_color(
        c("white", "white", "white",
          "red", "pink", "orange", "cyan")) |> 
    e_datazoom() |> 
    e_zoom(
        dataZoomIndex = 0,
        start = max(daily$date) - 365,
        end = max(daily$date),
        btn = "BUTTON"
    ) |> 
    e_button(
        id = "BUTTON", 
        htmltools::tags$i(class = "fa fa-search"), # passed to the button
        class = "btn btn-default",
        "1Y"
    ) |> 
    
    e_title("Candlestick chart", "Alpha Vantage data") |> 
    e_theme("dark") |>   # theme
    e_tooltip(
        axisPointer = list(
            type = "cross"
        )) 





