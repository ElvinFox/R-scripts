###############################################################
# collecting info from CoinMarketCap                          #
# information about prices and Market caps of ICOs            #
###############################################################

library('rvest')

Token <- "cashaa"
p_1 <- "https://coinmarketcap.com/currencies/"
p_2 <- "/historical-data/?start=20130428&end=20191220"

path_ <- paste(p_1, Token, p_2 , sep = "")

url <- (path_)

prices <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
    html_table()

prices <- prices[[1]]


### Data cleaning   ###

prices$Volume <- gsub( "\\,", "", prices$Volume )
prices$Volume <- as.double(prices$Volume)

prices$`Market Cap` <- gsub( "\\,", "", prices$`Market Cap`  )
prices$`Market Cap`  <- as.double(prices$`Market Cap` )


## statistics collection
OpenFirstDayPrice <- prices[ length(prices[,1]), 2]
CloseFirstDayPrice <- prices[ length(prices[,1]), 5]
Max_price <- max(prices$`Close**`, na.rm = T)
Min_price <- min(prices$`Close**`, na.rm = T)
    
Max_MarketCap <-  max(prices$`Market Cap`, na.rm = T)
Current_MarketCap <- prices$`Market Cap`[1]


#write.csv2(reg, file = "Regs.csv")

