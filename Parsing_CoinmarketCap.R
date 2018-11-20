library('rvest')
library(dplyr)
library(plotly)

Token <- "augur"
p1 <- "https://coinmarketcap.com/currencies/"
p2 <- "/#markets"

path <- paste(p1, Token, p2 , sep = "")

url <- (path)

markets <- url %>%
    html() %>%
    html_nodes(xpath='//*[@id="markets-table"]') %>%
    html_table()

markets <- markets[[1]]


### Data cleaning   ###

#change Exchanges to Factors
markets$Source <- as.factor(markets$Source)

#remove * from trading volume
markets$`Volume (24h)` <- gsub("[[:punct:]]", "", markets$`Volume (24h)`)
markets$`Volume (24h)` <- as.integer(markets$`Volume (24h)`)

#remove $,* from price
markets$Price <- gsub( "\\$", "", markets$Price )
markets$Price <- gsub( "\\*", "", markets$Price )
markets$Price  <- as.double (markets$Price)

#convert Volume(%) to %
markets$`Volume (%)` <- substr(markets$`Volume (%)`,1,nchar(markets$`Volume (%)`)-1)
markets$`Volume (%)` <- as.double(markets$`Volume (%)`)


### Statistics  ###

#number of exchanges 
exs <- paste("The " , toupper(Token), " is traded on ", length(unique(markets$Source)), " exchanges" , sep = "")

#top 5 exchnages
top_5_ex <- markets %>% 
    group_by(Source) %>% 
    summarise( share = sum(`Volume (%)`) ) %>% 
    arrange(desc(share)) %>% 
    top_n(5)
top_5_exchanges_share <- sum(top_5_ex$share)
top_5_exchanges_names <- as.character(top_5_ex$Source)

top_5_ex$Source <- as.character(top_5_ex$Source)
shr <- 100-top_5_exchanges_share
oth <- c("Other", shr)
top_5_ex[6,] <- oth


p_ex <- plot_ly(top_5_ex, labels = ~top_5_ex$Source, values = ~as.double(top_5_ex$share), type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = 'Top 5 Exchanges Shares',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

top_e <- paste("The top exchange for ",toupper(Token), " is ", top_5_exchanges_names[1], " with the share of ", top_5_ex$share[1], "%",sep = "")

#top 5 currencies

#distinguish Currencies
    splt <- strsplit(markets$Pair,split='/', fixed=TRUE)
    splt <- unlist(splt)
    markets$Pair <- splt[seq(2, length(splt), by = 2)] 
    markets$Pair <- as.factor(markets$Pair)
    
    top_5_cur <- markets %>% 
        group_by(Pair) %>% 
        summarise( share = sum(`Volume (%)`) ) %>% 
        arrange(desc(share)) %>% 
        top_n(5)
    
    top_5_cur_share <- sum(top_5_cur$share)
    top_5_cur_names <- as.character(top_5_cur$Pair)
    
    top_5_cur$Pair <- as.character(top_5_cur$Pair)
    shr_cur <- 100-top_5_cur_share
    oth <- c("Other", shr_cur)
    top_5_cur[6,] <- oth
    
    
   p_cur <-  plot_ly(top_5_cur, labels = ~top_5_cur$Pair, values = ~as.double(top_5_cur$share), type = 'pie') %>%
        layout(title = 'Top 5 Currencies Shares',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
top_c <- paste("The top the ",  "currency the ",toupper(Token), " is traded aginst is ", top_5_cur_names[1], " its share is ", top_5_cur$share[1], "%",sep = "")

### Final print out ###
print(markets)
print( exs )
print(top_e)
print(top_c)
p_ex
p_cur








### current financials

Token <- "zcash"
p_1 <- "https://coinmarketcap.com/currencies/"
p_2 <- "/historical-data/?start=20130428&end=20180914"

path_ <- paste(p_1, Token, p_2 , sep = "")

url <- (path_)

prices <- url %>%
    html() %>%
    html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
    html_table()

prices <- prices[[1]]

#cleaning
prices$Volume <- gsub( "\\,", "", prices$Volume )
prices$Volume <- as.double(prices$Volume)

prices$`Market Cap` <- gsub( "\\,", "", prices$`Market Cap`  )
prices$`Market Cap`  <- as.double(prices$`Market Cap` )


#creating matrix
matrix <- matrix(1:9, nrow = 3, 
                 dimnames = list(c("Price","Market Cap","Trading Volume"), 
                                 c("Current","Minimum","Maximum")))

#price statistics
matrix[1,1] <- paste("$",format( round(prices$`Close**`[1],1), scientific=FALSE), sep = "")
matrix[1,2] <- paste("$",format(round(min(prices$`Close**`),1), scientific=FALSE), sep = "")
matrix[1,3] <- paste("$",format(round(max(prices$`Close**`),1), scientific=FALSE), sep = "")

#market cap statistics


x <- nchar ( min(prices$`Market Cap`) )
n <- 10**nchar ( min(prices$`Market Cap`) )


if (x<3) {
    V <<-''
}else if (x>=3 && x<6){
    V <<-'k'
}else if (x>=6 && x<9){
    V <<-'M'
}else if (x>=9 ){
    V <<-'bln'
}

matrix[2,1] <- paste("$",format(round(prices$`Market Cap`[1]/n,1), scientific=FALSE), V, sep = "")
matrix[2,2] <- paste("$",format(round(min(prices$`Market Cap`)/n,1), scientific=FALSE), V, sep = "")
matrix[2,3] <- paste("$",format(round(max(prices$`Market Cap`)/n,1), scientific=FALSE), V, sep = "")

#trading volume statistics
x <- nchar ( min(prices$Volume) )
n <- 10**nchar ( min(prices$Volume) )


if (x<3) {
    V <<-''
}else if (x>=3 && x<6){
    V <<-'k'
}else if (x>=6 && x<9){
    V <<-'M'
}else if (x>=9 ){
    V <<-'bln'
}

matrix[3,1] <- paste("$",format(round(prices$Volume[1]/n,1), scientific=FALSE), V, sep = "")
matrix[3,2] <- paste("$",format(round(min(prices$Volume)/n,1), scientific=FALSE), V, sep = "")
matrix[3,3] <- paste("$",format(round(max(prices$Volume)/n,1), scientific=FALSE), V, sep = "")

matrix

tkn_price <- 100
cnt_price <- prices$`Close**`[1]
ROI <- paste( (cnt_price-tkn_price)/tkn_price * 100, "%", sep = "")
