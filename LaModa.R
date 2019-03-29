###############################

# parsing LM                #

###############################

library('rvest')
library(dplyr)
library(tibble)
library(lubridate)   #important library for date manipulations


############### GIRLS Clothes######################

number_of_SKUs <- 10455
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/1590/clothes-dlia-devochek/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    

    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "girls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Clothes"


write.csv2(Parsed, file = "Lamoda_girls_Clothes.csv")


############### GIRLS Shoes######################

number_of_SKUs <- 3109
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/203/shoes-girls/?property_season_wear=5595%2C5594%2C5592'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '&page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "girls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Shoes"
Parsed$Season <- "demi=5595;summer=5594;mult=5592"


write.csv2(Parsed, file = "Lamoda_girls_Shoes.csv")



############### GIRLS Acessories######################

number_of_SKUs <- 1282
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/561/accs-detskieaksessuary/?property_season_wear=5595%2C5594%2C5592'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '&page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "girls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Acessories"


write.csv2(Parsed, file = "Lamoda_girls_Acessories.csv")


############### GIRLS School######################

number_of_SKUs <- 659
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/6176/default-devochkishkola/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '&page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "girls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "School"
write.csv2(Parsed, file = "Lamoda_girls_School.csv")


############### GIRLS Sport######################

number_of_SKUs <- 745
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/1874/default-sports-forgirls/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '&page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "girls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Sport"
write.csv2(Parsed, file = "Lamoda_girls_Sport.csv")



#####################
############### BOYS Clothes######################

number_of_SKUs <- 7504
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/1589/clothes-dlia-malchikov/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "boys"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Clothes"


write.csv2(Parsed, file = "Lamoda_boys_Clothes.csv")






############### BOYS Shoes######################

number_of_SKUs <- 3302
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/205/shoes-boys/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "boys"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Shoes"


write.csv2(Parsed, file = "Lamoda_boys_Shoes.csv")




############### BOYS Acessories######################

number_of_SKUs <- 977
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/5381/default-aksydlyamalchikov/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "boys"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Acessories"


write.csv2(Parsed, file = "Lamoda_boys_Acessories.csv")



############### BOYS School######################

number_of_SKUs <- 453
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/6187/default-malchikishkola/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "boys"
Parsed$Store <- "Lamoda"
Parsed$Level <- "School"


write.csv2(Parsed, file = "Lamoda_boys_School.csv")



############### BOYS Sport######################

number_of_SKUs <- 1139
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/1875/default-sports-forboys/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "boys"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Sport"


write.csv2(Parsed, file = "Lamoda_boys_Sport.csv")




#####################
############### NB Clothes######################

number_of_SKUs <- 2304
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/5598/clothes-newbornclothes/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "NB"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Clothes"


write.csv2(Parsed, file = "Lamoda_NB_Clothes.csv")

############### NB Shoes######################

number_of_SKUs <- 237
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/5599/shoes-kidsnewborn/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "NB"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Shoes"


write.csv2(Parsed, file = "Lamoda_NB_Shoes.csv")

############### NB Acessories######################

number_of_SKUs <- 89
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/5600/accs-newbornaccs/'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()

for(i in 1:pages) {
    url <- paste0(input, '?page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,15),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "NB"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Acessories"


write.csv2(Parsed, file = "Lamoda_NB_Acessories.csv")



############# saving images ###############

DF$Images <- as.character(DF$Images)



y = paste0("http:", DF$Images[1])
download.file(y, 'y.jpg', mode = 'wb')

DF$Images[1]

WD <- paste0(WD_L1, '/', Sex, '/', Level, '/', Type)



## Create dirs
WD_L1 <- "C:/Users/diu/Documents/LM/Images/"

for (i in levels(DF$Sex)) {
    
    dir.create(file.path(WD_L1, i))
    ln_1 <- paste0(WD_L1, i, "/")
    
    lvl <- DF %>%
        filter(Sex == i) %>%
        select(Level) %>%
        distinct() %>%
        pull
    
    as.character(lvl)
    
    for (n in lvl) {
        
        dir.create(file.path(ln_1, n))
        ln_2 <- paste0(ln_1, n, "/")
        
       tp <- DF %>%
            filter(Sex == i & Level == n) %>%
            select(Type) %>%
            distinct() %>%
            pull
        
        as.character(tp)
        
        for (m in tp) {
            
            dir.create(file.path(ln_2, m), showWarnings = FALSE)
        }
    }
}



DF <- DF[-grep("Артикул:", DF$Types),] 

WD_L1 <- "C:/Users/diu/Documents/LM/Images/"
x <-  as.numeric(nrow(DF))

Start_time <- Sys.time()
for (z in 28420:x) {

    WD <- paste0(WD_L1, '/', DF$Sex[z], '/', DF$Level[z], '/', DF$Type[z])
    setwd(WD)
    
    y = paste0("http:", DF$Images[z])
    download.file(y, paste0(DF$Articles[z], ".jpg"), mode = 'wb')

    if ((z %% 2) == 0)
        Sys.sleep(round(runif(1, 1, 2), 0))
    
    if ((z %% 6) == 0)
        print (
            paste0("i = ", z, " Done ", round(z / x * 100, 3), "%")
        )
    if ((z %% 50) == 0)
        Sys.sleep(round(runif(1, 2, 4), 0))
}

# 28419 DOne

DF[28419,]






### winter

number_of_SKUs <- 716
pages <- ceiling( number_of_SKUs/60 )

input <-
    'https://www.lamoda.ru/c/203/shoes-girls/?property_season_wear=5593'  # <- starting link

Brands <- list()
Types <- list()
Prices <- list()
Articles <- list()
Images <- list()


for(i in 1:pages) {
    url <- paste0(input, '&page=', i)
    LM <- url %>% read_html()
    
    # brand
    brand <- LM %>%
        html_nodes(".products-list-item__brand") %>%
        html_text()
    brand <- as.character(gsub("\\ /.*", "", brand))
    
    # type of the product
    type <- LM %>%
        html_nodes(".products-list-item__img")
    
    type <- as.character(gsub(".*alt", "", type))
    type <- substring(type, 3)
    type <- gsub("\\,.*", "", type)
    
    #product article
    article <- LM %>%
        html_nodes(".products-list-item__img")
    article <- as.character(gsub(".*Артикул: ", "", article))
    article <- as.character(gsub("\\..*", "", article))
    
    #product image
    img <- LM %>%
        html_nodes(".products-list-item__img") %>% 
        html_attr("src")
    
    
    # prices old -> after discount
    price <- LM %>%
        html_nodes(".price") %>%
        html_text()
    
    Brands <- rbind(Brands, brand)
    Types <- rbind(Types, type)
    Prices <- rbind(Prices, price)
    Articles <- rbind(Articles, article)
    Images <- rbind(Images, img)
    
    print (paste0( "i = ", i, " Done ", round(i/pages,3)*100, "%"))
    if( (i %% 2) == 0 ) Sys.sleep(round(runif(1,2,6),0))
    if( (i %% 20) == 0 ) print(length(Brands))
}

Brands <- unlist(Brands)
Types <- unlist(Types)
Prices <- unlist(Prices)
Articles <- unlist(Articles)
Images <- unlist(Images)

Parsed <- data.frame(Brands, Types, Prices, Articles, Images,  stringsAsFactors = FALSE) 
Parsed$Sex <- "bgirls"
Parsed$Store <- "Lamoda"
Parsed$Level <- "Shoes"
Parsed$Season <- "Winter"

write.csv2(Parsed, file = "Lamoda_girls_Shoes_winter.csv")



write.csv2(Parsed, file = "Lamoda_girls_Shoes_winter.csv")



save(DF,file="LM_DF.Rda")
load("LM_DF.Rda")