
####    download list of all listed tokens by date
library(anytime)
library(tidyverse)
library(jsonlite)
library('rvest')

start_n <- 0
site <- paste0("https://api.coinmarketcap.com/v2/ticker/?start=", 100*start_n + 1, "&limit=100", "&sort=rank")

tokens_list <- site %>% html() 
tokens_list <- read_json(site)

Listed_tokens <- tokens_list[[1]]
test <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[2]])
test1 <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[4]])
NF <- as.data.frame(cbind(test, test1))

start_n <- 1

tokens_list <- site %>% html() 
tokens_list <- read_json(site)

Listed_tokens <- tokens_list[[1]]
test <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[2]])
test1 <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[4]])
NF1 <- as.data.frame(cbind(test, test1))

start_n <- 2

tokens_list <- site %>% html() 
tokens_list <- read_json(site)

Listed_tokens <- tokens_list[[1]]
test <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[2]])
test1 <- sapply(1:100, function(x) unlist(Listed_tokens[x])[[4]])
NF2 <- as.data.frame(cbind(test, test1))

NF <- rbind(NF, NF1, NF2)
NF <- cbind(NF, NF$test, NF$test1)


DF <- NF

####    parcing by token from CoinMarketCap

NR<- nrow(DF)
# NR <- 100
new_DF <- DF[0,c(1:3)]

start_time <- anytime(as.ordered(date()))

for (i in 1 : NR) {
    
    Token <- DF[i, 4]
    tkn_real_name <- DF[i,3]
    p_1 <- "https://coinmarketcap.com/currencies/"
    p_2 <- "/historical-data/?start=20181112&end=20181118"
    
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
    if( (i %% 50) ==0 ) check_point_time <- anytime(as.ordered(date()))
    if( (i %% 50) ==0 ) bot$sendMessage(chat_id = chat_id, text = paste0( date(), "\n", 
                                                                          "Done ", round(i/NR,8)*100, "% ", "\n", 
                                                                          'Last successfull number is --->  ', i, " out of   ", NR, "\n",
                                                                          'ETA -> ', check_point_time + (check_point_time-start_time)/round(i/NR,3)) )
    if( (i %% 10) ==0 ) Sys.sleep(25)
    
    if(i == NR) bot$sendAnimation(chat_id = chat_id,
                                  animation = "https://media.giphy.com/media/sIIhZliB2McAo/giphy.gif")
    
    if(i == NR) write.csv2(new_DF, file = "Token_volatility.csv")
    if(i == NR) bot$send_document( chat_id = chat_id, document = "Token_volatility.csv")
    if(i == NR) now <- anytime(as.ordered(date()))
    if(i == NR) pass <- now-start_time
    if(i == NR) bot$sendMessage( chat_id = chat_id, text = paste0(pass) )
}

