###############################################################
# collecting info from CoinMarketCap                          #
# information about prices and Market caps of ICOs            #
###############################################################

library('rvest')

DF <- read.csv("~/Desktop/WW_Listings.csv", sep = ";", stringsAsFactors = F )

NR<- nrow(DF)
####30 DONE

for (i in 684:NR) {

    Token <- DF[i, 5]
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
    DF$OpenFirstDayPrice[i] <- prices[ length(prices[,1]), 2]
    DF$CloseFirstDayPrice[i] <- prices[ length(prices[,1]), 5]
    DF$Max_price[i] <- max(prices$`Close**`, na.rm = T)
    DF$Min_price[i] <- min(prices$`Close**`, na.rm = T)
        
    DF$Max_MarketCap[i] <-  max(prices$`Market Cap`, na.rm = T)
    DF$Current_MarketCap[i] <- prices$`Market Cap`[1]
    
    print (paste0("Done, last number is ", i))
    timestamp()
    
    print (paste0("Done ", round(i/NR,3)*100, "%"))
    if( (i %% 30) ==0 ) Sys.sleep(60)
     
}

write.csv2(DF, file = "CMC_stats_update.csv")

