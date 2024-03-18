
i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart ----

plot_candlestick <- function(df,
                             start_date = NULL,
                             end_date = NULL,
                             name = NULL,
                             noeud_mm = FALSE,
                             gmma = FALSE){
    
    min_date <-min(df$date)
    max_date <- max(df$date)
    
    if(!missing(start_date) && !missing(end_date)){
        
        df <- df[between(date,
                         as.Date(start_date),
                         as.Date(end_date))]
        
    } else if(missing(start_date) && !missing(end_date)){
        
        df <- df[between(date,
                         as.Date(min_date),
                         as.Date(end_date))]
        
    } else if(!missing(start_date) && missing(end_date)){
        
        df <- df[between(date,
                         as.Date(start_date),
                         as.Date(max_date))]
    }
    
    plot <- df %>% plot_ly(x = ~date, type="candlestick",
                           open = ~open, close = ~close,
                           high = ~high, low = ~low, name = name,
                           increasing = i, decreasing = d) %>% 
        
        add_lines(x = ~date, y = ~bol_sup , name = "B Bands",
                  line = list(color = 'black', width = 0.5),
                  legendgroup = "Bollinger Bands",
                  hoverinfo = "none", inherit = F) %>%
        
        add_lines(x = ~date, y = ~bol_inf, name = "B Bands",
                  line = list(color = 'black', width = 0.5),
                  legendgroup = "Bollinger Bands", inherit = F,
                  showlegend = FALSE, hoverinfo = "none") %>%
        
        add_lines(x = ~date, y = ~mavg, name = "Mv Avg",
                  line = list(color = '#E377C2', width = 0.5),
                  hoverinfo = "none", inherit = F)  %>%
        
        add_lines(x = ~date, y = ~wma4 , name = "WMA4",
                  line = list(color = 'red', width = 0.5),
                  hoverinfo = "none", inherit = F) %>%
        
        add_lines(x = ~date, y = ~sma200 , name = "SMA200",
                  line = list(color = 'red', width = 1),
                  hoverinfo = "none", inherit = F) %>%
        
        add_lines(x = ~date, y = ~keltner_dn , name = "Keltner down",
                  line = list(color = 'purple', width = 1),
                  hoverinfo = "none", inherit = F) 
    
    
    # add_lines(x = ~date, y = ~wma12 , name = "WMA12",
    #               line = list(color = 'red', width = 0.5),
    #               hoverinfo = "none", inherit = F) %>% 
    # 
    # add_lines(x = ~date, y = ~ema21, name = "EMA21",
    #           line = list(color = 'blue', width = 0.5), inherit = F,
    #           hoverinfo = "none") %>% 
    # 
    # add_lines(x = ~date, y = ~sma50, name = "SMA50",
    #           line = list(color = '#E377C2', width = 0.5),
    #           hoverinfo = "none", inherit = F)  %>% 
    # 
    # add_lines(x = ~date, y = ~wma150, name = "WMA150",
    #           line = list(color = '#E377C2', width = 0.5),
    #           hoverinfo = "none", inherit = F)  %>% 
    # 
    # add_lines(x = ~date, y = ~sma200, name = "SMA200",
    #           line = list(color = '#E377C2', width = 0.5),
    #           hoverinfo = "none", inherit = F)  %>% 
    
    # add_lines(x = ~date, y = ~safeline, name = "Safeline",
    #           line = list(color = 'green', width = 0.5),
    #           hoverinfo = "none", inherit = F)  %>% 
    # 
    # add_lines(x = ~date, y = ~safelinelow, name = "SafelineLow",
    #           line = list(color = '#lightgreen', width = 0.5),
    #           hoverinfo = "none", inherit = F)  %>% 
    
    
    
    if(!missing(gmma) | isTRUE(gmma)){
        
        plot <-  plot %>% 
            
            add_lines(x = ~date, y = ~ema3 , name = "ema3",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema5 , name = "ema5",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema8 , name = "ema8",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema10 , name = "ema10",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema12 , name = "ema12",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema15 , name = "ema15",
                      line = list(color = 'blue', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema30 , name = "ema30",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema35 , name = "ema35",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema40 , name = "ema40",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema45 , name = "ema45",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema50 , name = "ema50",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) %>%
            
            add_lines(x = ~date, y = ~ema60 , name = "ema60",
                      line = list(color = 'green', width = 0.5),
                      hoverinfo = "none", inherit = F) 
        
    }
    
    if(!missing(noeud_mm) | isTRUE(noeud_mm)){
        
        plot <-  plot %>% 
            
            add_trace(x = ~date, y = ~wma12, type = 'scatter', mode = 'lines',
                      line = list(color = 'rgba(0,100,80,1)'),
                      showlegend = FALSE, name = 'WMA12')
        
        
        plot <- plot %>% 
            
            add_trace(x = ~date, y = ~ema21, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
                      showlegend = FALSE, name = 'EMA21')
        
        # 
        # add_lines(x = ~date, y = ~ema21 , name = "ema21",
        #           line = list(color = 'blue', width = 0.5),
        #           hoverinfo = "none", inherit = F) %>%
        #     
        #     add_lines(x = ~date, y = ~sma50 , name = "sma50",
        #               line = list(color = 'blue', width = 0.5),
        #               hoverinfo = "none", inherit = F)
        
    }
    
    
    return(plot |> 
               layout(title = "Basic Candlestick Chart",
                      xaxis = list(rangeslider = list(visible = F))))
    
}

# plot volume bar chart ----

plot_volume  <- function(df,
                         start_date = NULL,
                         end_date = NULL,
                         name = NULL) {
    
    min_date <-min(df$date)
    max_date <- max(df$date)
    
    if(!missing(start_date) && !missing(end_date)){
        
        df <- df[between(date,
                         as.Date(start_date),
                         as.Date(end_date))]
        
    } else if(missing(start_date) && !missing(end_date)){
        
        df <- df[between(date,
                         as.Date(min_date),
                         as.Date(end_date))]
        
    } else if(!missing(start_date) && missing(end_date)){
        
        df <- df[between(date,
                         as.Date(start_date),
                         as.Date(max_date))]
    }
    
    plot <- df %>%
        plot_ly(x=~date, y=~Volume, type='bar', name = name,
                colors = c('#17BECF','#7F7F7F')) 
    
    plot <- plot %>% layout(yaxis = list(title = "Volume"))
    
    return(plot)
    
    
}

# Generate plot ----

generate_plot <- function(start_date = NULL, 
                          end_date = NULL) {
    
    if(missing(start_date) && missing(end_date)){
        
        plot_daily <- plot_candlestick(daily,
                                       name = input,
                                       noeud_mm = FALSE)
        
        plot_weekly <- plot_candlestick(weekly,
                                        name = input,
                                        noeud_mm = FALSE)
        
        plot_monthly <- plot_candlestick(monthly,
                                         name = input,
                                         noeud_mm = FALSE)
        
    } else if(missing(start_date) && !missing(end_date)) {
        
        
        plot_daily <- plot_candlestick(daily,
                                       end_date = end_date,
                                       name = input,
                                       noeud_mm = FALSE)
        
        plot_weekly <- plot_candlestick(weekly,
                                        end_date = end_date,
                                        name = input,
                                        noeud_mm = FALSE)
        
        plot_monthly <- plot_candlestick(monthly,
                                         end_date = end_date,
                                         name = input,
                                         noeud_mm = FALSE)
        
    } else if(!missing(start_date) && missing(end_date)) {
        
        
        plot_daily <- plot_candlestick(daily,
                                       start_date = start_date,
                                       name = input,
                                       noeud_mm = FALSE)
        
        plot_weekly <- plot_candlestick(weekly,
                                        start_date = start_date,
                                        name = input,
                                        noeud_mm = FALSE)
        
        plot_monthly <- plot_candlestick(monthly,
                                         start_date = start_date,
                                         name = input,
                                         noeud_mm = FALSE)
        
    } else if(!missing(start_date) && !missing(end_date)) {
        
        
        plot_daily <- plot_candlestick(daily,
                                       start_date = start_date,
                                       end_date = end_date,
                                       name = input,
                                       noeud_mm = FALSE)
        
        plot_weekly <- plot_candlestick(weekly,
                                        start_date = start_date,
                                        end_date = end_date,
                                        name = input,
                                        noeud_mm = FALSE)
        
        plot_monthly <- plot_candlestick(monthly,
                                         start_date = start_date,
                                         end_date = end_date,
                                         name = input,
                                         noeud_mm = FALSE)
        
    }  
    
    # subplot with shared x axis
    plot_final <- subplot(plot_monthly |> 
                              layout(title = "Basic Candlestick Chart",
                                     xaxis = list(rangeslider = list(visible = F))),
                          plot_weekly |> 
                              layout(title = "Basic Candlestick Chart",
                                     xaxis = list(rangeslider = list(visible = F))),
                          plot_daily,
                          nrows=3,
                          shareX = TRUE,
                          titleY = TRUE)
    
    return(plot_final)
    
}
