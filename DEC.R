library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)
library(gtools)


# https://www.decathlon.ru/C-99470-odezhda-dlya-muzhchin 
# https://www.decathlon.ru/C-99483-zhenskaya-sportivnaya-odezhda 
# https://www.decathlon.ru/C-221794-detskaja-odezhda 
# 
# https://www.decathlon.ru/C-99473-obuv-muzhskaya 
# https://www.decathlon.ru/C-99485-obuv-zhenskaya 
# https://www.decathlon.ru/C-101232-obuv-dlja-detej 
# 
# https://www.decathlon.ru/C-1160849-aksessuary-dlya-muzhchin
# https://www.decathlon.ru/C-1161012-aksessuary-dlya-zhenshin  
# https://www.decathlon.ru/C-1161020-aksessuary-dlya-detei 



start_url <- c("https://www.dochkisinochki.ru/icatalog/categories/odezhda/",
               "https://www.dochkisinochki.ru/icatalog/categories/obuv/",
               "https://www.dochkisinochki.ru/icatalog/categories/shkolnaya-odezhda/")

DEC <- "https://www.decathlon.ru/C-99470-odezhda-dlya-muzhchin" %>% read_html()

SKU_nmbr <- DEC %>% 
    html_nodes(".filter-bar") %>% 
    html_nodes(".filter-results-number") %>% 
    html_text() %>% 
    as.numeric()


#https://www.decathlon.ru/C-99473-obuv-muzhskaya#page2
url <- paste0(start_url, "#page", i)
DEC <- url %>% read_html()



prod_href <- 
    DEC %>% 
    html_nodes(".thumbnails-list li a") %>%  html_attr("href")
prod_href <- paste0("https://www.decathlon.ru", prod_href)

prod_img <- 
    DEC %>% 
    html_nodes(".thumbnails-list li a") %>%  
    html_nodes(".one-picture-container img") %>% html_attr("data-src")
prod_img <- paste0("https://www.decathlon.ru", prod_img)


front <- cbind(prod_href, prod_img)
front <- as.data.frame(front)





DEC <- "https://www.decathlon.ru/krossovki-sc500-mid-vzr-_e3-id_8497467.html" %>% read_html()


###### collecting by SKU ####
prod_href <- i

prod_name <- 
    DEC %>% 
    html_nodes(".product-page-by-floor") %>% 
    html_nodes(".clear h1") %>%  html_text()

df <- cbind(prod_name)

tree <-
    DEC %>%
    #html_nodes(".ComponentBreadcrumb") %>%
    html_nodes(".breadcrumb-wrapper") %>%
    html_nodes(".breadcrumb-label ") %>%
    html_text()
tree <- gsub( "[\t\r\n]", "", tree)

for(u in 1:length(tree) ){
    df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
    colnames(df)[ncol(df)]<- paste0("lvl_",u)
}

df <- as.data.frame(df)


price <- 
    DEC %>% 
    html_nodes(".box_price") %>% 
    html_nodes(".price") %>% 
    #html_attrs()
    html_text()
price <- price[2]
price_old <- unlist( strsplit(price, ".", fixed = TRUE) )
price_old <- as.numeric( gsub("[^0-9]","", price_old[1]) )
price_new <- unlist( strsplit(price, ".", fixed = TRUE) )
price_new <- as.numeric( gsub("[^0-9]","", price_new[2]) )


characteristics_2 <- 
    DEC %>% 
    html_nodes(".block-description div") %>% 
    html_nodes(".libelle-description") %>% 
    html_text()

characteristics_1 <- 
    DEC %>% 
    html_nodes(".block-description div") %>% 
    html_nodes(".title-description") %>% 
    html_text()

chrs <- cbind(characteristics_1, characteristics_2)
chrs <- as.data.frame(chrs)


for(p in 1:nrow(chrs) ){
    df <-  cbind (df, assign(paste("Desc", p, sep = ""), chrs[p,2]))
    colnames(df)[ncol(df)]<- as.character(chrs[p,1])
}




adv_1 <- 
    DEC %>% 
    html_nodes(".secondary-product-container") %>% 
    html_nodes(".list_avantage div") %>% 
    html_nodes(".table") %>% 
    html_nodes(".title-description") %>% 
    html_text()
adv_1 <- as.character(gsub("[^A-zА-я]", "", adv_1)) 


adv_2 <- 
    DEC %>% 
    html_nodes(".secondary-product-container") %>% 
    html_nodes(".list_avantage div") %>% 
    html_nodes(".table") %>% 
    html_nodes(".libelle-description") %>% 
    html_text()
adv_2 <- as.character(gsub("\n   ", "", adv_2)) 

adv <- cbind(adv_1, adv_2)
adv <- as.data.frame(adv)


for(q in 1:nrow(adv) ){
    df <-  cbind (df, assign(paste("Adv", q, sep = ""), adv[q,2]))
    colnames(df)[ncol(df)]<- as.character(adv[q,1])
}

