library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
library(sjmisc)
library(gtools)




# # Clothes
# 
#     https://www.sportmaster.ru/catalog/muzhskaya_odezhda/
#     https://www.sportmaster.ru/catalog/zhenskaya_odezhda/
#     https://www.sportmaster.ru/catalog/odezhda_dlya_malchikov/ 
#     https://www.sportmaster.ru/catalog/odezhda_dlya_devochek/ 
# # Shoes
#     https://www.sportmaster.ru/catalog/muzhskaya_obuv/ 
#     https://www.sportmaster.ru/catalog/zhenskaya_obuv/ 
#     https://www.sportmaster.ru/catalog/obuv_dlya_malchikov/ 
#     https://www.sportmaster.ru/catalog/obuv_dlya_devochek/ 
#


# https://www.sportmaster.ru/


list_urls <- 
    SMR %>% 
    html_nodes(".newSubmenuTitle a") %>% 
    html_text()








# ?pageSize=120



front <- data.frame()
begin <- Sys.time()


# response delay from the website

library(httr)
for(my_url in my_urls){
    t0 <- Sys.time()
    response <- httr::GET(my_url)
    t1 <- Sys.time()
    #do something
    response_delay <- as.numeric(t1-t0)
    Sys.sleep(10*response_delay) # sleep 10 times longer than response_delay
}

input <- "https://www.sportmaster.ru/catalog/muzhskaya_odezhda/"

url <- paste0(input, "?pageSize=120&page=",i )
SMR <- url %>% read_html()

prod_img <- 
    SMR %>% 
    html_nodes(".sm-category__item-photo img") %>% 
    html_text()


prod_name <- 
    SMR %>% 
    html_nodes(".sm-category__item h2 a") %>% 
    html_text()

prod_id <- 
    SMR %>% 
    html_nodes(".sm-category__item-photo a") %>% 
    html_text()


# https://www.sportmaster.ru/product/10332081/

paste0("https://www.sportmaster.ru", prod_id)

sf <- cbind(prod_id, prod_img, prod_category, prod_link)
sf <- as.data.frame(sf)
front <- smartbind(front, sf)







tree <- 
    SMR %>% 
    html_nodes(".sm-breadcrumbs") %>% 
    html_text()


tree <- gsub("[\t \n]", "", tree)



# price main, old

price_new <- 
    SMR %>% 
    html_nodes(".sm-amount") %>% 
    html_text()
price_new <- as.numeric( gsub("[^0-9]", "", price_new[1]) )

price_old <- 
    SMR %>% 
    html_nodes(".wr_item_3_old_price") %>% 
    html_text()
price_old <- as.numeric( gsub("[^0-9]", "", price_old[1]) )


sizes <- 
    SMR %>% 
    html_nodes(".clearfix div") %>% 
    html_nodes(".sm-goods__param-value") %>% 
    html_text()


descript <- 
    SMR %>%
    html_nodes (".sm-goods__description-text")

characteristics <- 
    SMR %>%
    html_nodes (".sm-goods_tabs-bodies_body") %>% 
    html_nodes (".sm-goods_tabs_characteristix") %>% 
    html_nodes (".characteristics_values") %>% 
    html_table ()

color <- 
    SMR %>%
    html_nodes (".sm-goods_main_details_color a") %>% 
    html_text()
