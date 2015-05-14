library(data.table)
library(rvest)
library(stringr)

GetIntradayChanges <- function(){
  ### Download google finance main page
  page <- html("http://www.google.com/finance")

  ### US Market Indices
  mkts.us   <- data.table(
    html_table(
      html_node(page,"table#sfe-mktsumm")))
  mkts.us <- mkts.us[, list(index_name = X2, price_change = X4, per_change = X5)]
  mkts.us[, per_change := as.numeric(str_replace_all(per_change, "[()%]", ""))]

  ### International Market Indices
  mkts.intl <- data.table(
    html_table(
      html_node(page, "#markets div.sfe-section table.quotes"),
      fill=TRUE))
  mkts.intl <- mkts.intl[!is.na(X1) & X3 != "", list(index_name = X1, change = X3)]
  mkts.intl[, price_change :=
              as.numeric(
                unlist(
                  str_split(
                    str_replace_all(change,",",""),
                    " "))[1]),
            by=index_name]

  mkts.intl[, per_change :=
              as.numeric(
                unlist(
                  str_split(
                    str_replace_all(change,"[()%]", ""),
                    " "))[2]),
            by=index_name]
  mkts.intl[, change := NULL]

  ### Currency Exchange Rates
  currency  <- data.table(
    html_table(
      html_node(page,"#currencies div.sfe-section table.quotes")))
  currency <- currency[!is.na(X1) & X3 != "", list(index_name = X1, change = X3)]
  currency[, price_change :=
             as.numeric(
               unlist(
                 str_split(
                   str_replace_all(change,",",""),
                   " "))[1]),
           by=index_name]

  currency[, per_change :=
             as.numeric(
               unlist(
                 str_split(
                   str_replace_all(change,"[()%]", ""),
                   " "))[2]),
           by=index_name]
  currency[, change := NULL]


  ### Biggest Changes among US-traded stocks with Market Cap > $1B
  stocks <- data.table(
    html_table(
      html_node(page,"#tm_price_0 table"),
      fill=TRUE))

  stocks <- stocks[!(X1 %in% c("Gainers","Losers","")),
                   list(ticker = X1,
                        index_name = X2,
                        per_change = X3)]
  stocks[, per_change := as.numeric(str_replace_all(per_change,"%",""))]


  changes <- rbindlist(list(mkts.us, mkts.intl, currency, stocks),
                       fill = TRUE, use.names = T)

  # tickers <- data.table(index_name = c("Dow Jones", "S&P 500", "Nasdaq",
  #                                      "Shanghai", "Nikkei 225", "Hang Seng Index",
  #                                      "TSEC", "FTSE 100", "EURO STOXX 50", "CAC 40", "S&P TSX", "S&P/ASX 200",
  #                                      "BSE Sensex", "TA25", "SMI", "ATX", "IBOVESPA", "SET", "BIST100", "IBEX", "WIG", "MERVAL",
  #                                      "EUR/USD", "USD/JPY", "GBP/USD", "USD/CAD", "USD/HKD", "USD/CNY", "AUD/USD"),
  #                       ticker     = c(".DJI",".INX",".IXIC","SHA:000001","INDEXNIKKEI:NI225","INDEXHANGSENG:HSI")


  return(changes)
}




