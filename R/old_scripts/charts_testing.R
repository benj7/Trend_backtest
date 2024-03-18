library(dygraphs)

dyHide <-function(dygraph) {
    dyPlugin(
        dygraph = dygraph,
        name = "Hide",
        path = system.file("plugins/hide.js", package = "dygraphs")
    )
}

dygraph(cbind(ldeaths, mdeaths, fdeaths)) %>% 
    dyRangeSelector() %>% 
    dyHide()

# Charts dygraphs 

plot_candlestick_dy <- function(data, 
                                dateWindow = NULL){
    
    data <- copy(data)
    
    cols <- c("date", "open", "close", "low", "high", "mavg", "bol_inf", "bol_sup", "wma4", "wma12", "ema21", "sma50")
    
    data <- data[, .SD, .SDcols = cols]
    
    dygraph(data) |> 
        # dyOptions(stackedGraph = TRUE) %>% 
        dyCandlestickGroup(c('open', 'high', 'low', 'close')) %>% 
        dyRangeSelector(dateWindow = dateWindow) |> 
        dySeries("mavg", strokePattern = "dotted", color = "#2b90ff") |> 
        # dyCSScool() |> 
        dyUnzoom() |> 
        dyOptions(drawGrid = FALSE) |> 
        dyHighlight(highlightCircleSize = 2, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    
}

dateWindow <- c("2023-01-01", "2023-09-01")

test <- plot_candlestick_dy(daily,
                            dateWindow = dateWindow)
test
dyCSS(dygraph = test, css = "/Users/benj/Desktop/Montreux_backtest/dygraph.css")


library(quantmod)
library(dygraphs)
library(tidyverse)
library(lubridate)
library(htmlwidgets)
getSymbols("SPY", from="2016-01-01", to="2020-01-01")


SPY <- SPY[,c(1:4)] ## remove the volume and adjusted columns
SPY$SMA50 <- SMA(Cl(SPY), n = 50) #create SMA50 line
p <- dygraph(SPY, xlab = "Date", ylab = "Price", main = "SPY Price") %>%
    dySeries("SPY.Open", label = "Open", color = "black") %>%
    dySeries("SPY.Low", label = "Low", color = "red") %>%
    dySeries("SPY.High", label = "High", color = "green") %>%
    dySeries("SPY.Close", label = "Close", color = "orange") %>%
    dySeries("SMA50", label = "SMA50", color = "blue") %>%
    dyRangeSelector() %>%
    dyCandlestick()%>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = T)  %>%
    dyRoller(rollPeriod = 1)
p




library(quantmod)
tickers <- c("AAPL", "MSFT")
getSymbols(tickers)
closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
dateWindow <- c("2008-01-01", "2009-01-01")
p1 <- dygraph(closePrices, main = "Value", group = "stock") %>%
    dyRebase(value = 100) |>   
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE)
p2 <- dygraph(closePrices, main = "Percent", group = "stock") %>%
    dyRebase(percent = TRUE) 
p3 <- dygraph(closePrices, main = "None", group = "stock") %>%
    dyRangeSelector(dateWindow = dateWindow) |> 
    dyRibbon()
plotobj <- list(p1, p2, p3)

htmltools::browsable(htmltools::tagList(plotobj))


library(quantmod)
library(highcharter)

x <- getSymbols("GOOG", auto.assign = FALSE)
y <- getSymbols("AMZN", auto.assign = FALSE)

cols <- c("date", "open", "close", "low", "high")
data <- daily[, .SD, .SDcols = cols]
setnames(data, old = c("open", "close", "low", "high"),
         new = c("Open", "Close", "Low", "High"))

highchart(type = "stock") |> 
    hc_add_series(data = data) |> 
    hc_add_series( data = citytemp$tokyo
    ) |> 
    hc_add_series(
        name = "London", data = citytemp$london
    ) |> 
    hc_add_series(
        name = "Other city",
        data = (citytemp$tokyo + citytemp$london)/2
    )  |> 
    hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
    ) 



data <- adjustOHLC(SPY)

dygraph(nhtemp, main = "New Haven Temperatures") %>% 
    dySeries("V1", label = "Temperature (F)") %>%
    dyLegend(show = "always", hideOnMouseOut = TRUE)   
