
####    download list of all listed tokens by date

library(jsonlite)
library('rvest')
site <- "https://api.coinmarketcap.com/v2/listings/"

tokens_list <- site %>% html() 
tokens_list <- read_json(site)

Listed_tokens <- tokens_list[[1]]

number_of_tokens <- length( unlist(Listed_tokens) ) / 4

Matrix_for_parcing <- matrix( unlist(Listed_tokens), nrow =  number_of_tokens, ncol = 4, byrow = T)

DF <- Matrix_for_parcing

####    parcing by token from CoinMarketCap

NR<- nrow(DF)
# NR <- 3
new_DF <- DF[0,c(1:3)]

for (i in 1 : NR) {
    
    Token <- DF[i, 4]
    tkn_real_name <- DF[i,3]
    p_1 <- "https://coinmarketcap.com/currencies/"
    p_2 <- "/historical-data/?start=20100428&end=20201115"
    
    path_ <- paste(p_1, Token, p_2 , sep = "")
    url <- (path_)
    
    prices <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
        html_table()
    prices <- prices[[1]]
    
    number_of_days <- nrow(prices)
    
    prices_1 <- prices[,c(1,5)]
    prices_1[,3] <- tkn_real_name
    
    new_DF <- rbind(new_DF, prices_1)


    print (paste0("Done ", round(i/NR,3)*100, "%   ", date()))
    if( (i %% 50) ==0 ) bot$sendMessage(chat_id = chat_id, text = paste0( date(), "\n", "Done ", round(i/NR,3)*100, "% ", "\n", 'Last successfull number is --->  ', i, " out of   ", NR) )
    #if( (i %% 30) ==0 ) Sys.sleep(25)
    
    if(i == NR) bot$sendAnimation(chat_id = chat_id,
                      animation = "https://media.giphy.com/media/sIIhZliB2McAo/giphy.gif")
    
    if(i == NR) write.csv2(new_DF, file = "Token_volatility.csv")
    if(i == NR) bot$send_document( chat_id = chat_id, document = "Token_volatility.csv")

}



