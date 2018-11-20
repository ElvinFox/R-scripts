### TO DO
#install.packages("formattable")
library(formattable)

# 2) finish social medias
# 3) add facet grid to market pies
# 4) upadte date TO (right limit) - > can be used 2020 year
# 5) add list of all exchanges

setwd("~/Desktop/")
getwd()

library(rjson)
library(dplyr)

JF <- fromJSON(file = "~/Desktop/Listing_201_300", simplify = FALSE)

JF <- JF[[1]]

L <- length( unlist(JF) )

JF <- unlist(JF)

matrix <- matrix <- matrix(1:L, ncol =  4)

i <- 1
for (r in 1:2105){
    for (c in 1:4){
        matrix[r,c] <- JF[i]
        i <- i+1
    }
} 

matrix <- as.data.frame(matrix)

write.csv(matrix, file = "Listings.csv")


#collecting historical data from CMC
library('rvest')
p_1 <- "https://coinmarketcap.com/currencies/"
p_2 <- "/historical-data/?start=20130428&end=20181220"
Token <- "neo"

path_ <- paste(p_1, Token, p_2 , sep = "")

url <- (path_)

prices <- url %>%
    html() %>%
    html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
    html_table()

prices <- prices[[1]]

#write.csv(prices, file = "MyData.csv")

name <- paste (Token, ".csv", sep="")
write.csv(prices, file = name)




#   create file for parsing  (top 300 tokens)
test <- as.character(JF[[100]][2])


matrix <- matrix <- matrix(1:900, ncol =  3)


i <- 1
j <- 2

for (r in 1:300){
    for (c in 1:3){
        ifelse(j == 2 | j == 3,  j <- j + 1, j <- 2)
        matrix[r,c] <- as.character(JF[[i]][j])
    }
    i <- i+1
} 


i <- 201
j <- 2
t <- 1

for (r in 201:300){
    for (c in 1:3){
        ifelse(j == 2 | j == 3,  j <- j + 1, j <- 2)
        matrix[r,c] <- as.character(JF[[t]][j])
    }
    t <- t+1
} 


name <- paste ('top300', ".csv", sep="")
write.csv(matrix, file = name)



