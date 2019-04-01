library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
library(sjmisc)
library(gtools)


url_list <- list()

# starting urls
urls <- c("https://www.sportmaster.ru/catalog/muzhskaya_odezhda/",
          "https://www.sportmaster.ru/catalog/zhenskaya_odezhda/",
          "https://www.sportmaster.ru/catalog/odezhda_dlya_malchikov/",
          "https://www.sportmaster.ru/catalog/odezhda_dlya_devochek/",
          "https://www.sportmaster.ru/catalog/muzhskaya_obuv/ ",
          "https://www.sportmaster.ru/catalog/zhenskaya_obuv/",
          "https://www.sportmaster.ru/catalog/obuv_dlya_malchikov/",
          "https://www.sportmaster.ru/catalog/obuv_dlya_devochek/"
          )


# loop to get all child sub urls 
for(l in urls){
    
    SMR <- l %>% read_html()

     sub_url <-
         SMR %>%
         html_nodes(xpath = "/html/body/div[3]/div[2]/div[1]/nav/a") %>%
         html_attr('href')
     sub_url <- substr(sub_url, 10, nchar(sub_url))
    
     for( sub in sub_url){
         url_list <- rbind(url_list, paste0("https://www.sportmaster.ru/catalog/", sub))
         url_list <- unlist(url_list)
     }
     
}

url_list <- as.character(url_list)

SMR <- "https://www.sportmaster.ru/catalog/muzhskaya_odezhda/dzhempery_i_svitery/" %>% read_html()

# get number of SKUs

n_SKU <-
    SMR %>% 
    html_nodes(".sm-category__main-sorting_displayed") %>% html_text()
n_SKU <- as.numeric(gsub("[^0-9]", "", n_SKU)[1])

# ?pageSize=120

n_pages <- ceiling(n_SKU/120)

url <- paste0(url_list[1], "?pageSize=120&page=", 3)


front <- data.frame()
begin <- Sys.time()


# response delay from the website ~ 0.8 sec

library(httr)

    t0 <- Sys.time()
    response <- httr::GET(url)
    t1 <- Sys.time()
    response_delay <- as.numeric(t1-t0)
    response_delay



SMR <- url %>% read_html()

# need to remove recommend imgs as irrelevant
prod_img <- 
    SMR %>% 
    html_nodes(".sm-image-holder img") %>% 
    html_attr("src")
prod_img <- prod_img[nchar(prod_img) < 78]


prod_name <- 
    SMR %>% 
    html_nodes(".sm-category__item h2 a") %>% 
    html_text()

prod_ref <- 
    SMR %>% 
    html_nodes(".sm-category__item-photo a") %>% 
    html_attr("href")
prod_ref <- substr(prod_ref, 2, nchar(prod_ref)-1)



# variant for final SKU link
paste0("https://www.sportmaster.ru", prod_ref)

sf <- cbind(prod_img, prod_name, prod_ref)
sf <- as.data.frame(sf)
front <- smartbind(front, sf)



SMR <- "https://www.sportmaster.ru/product/10305440/" %>% read_html()
SMR <- "https://www.sportmaster.ru/product/10362293/" %>% read_html()

tree <- 
    SMR %>% 
    html_nodes(".sm-breadcrumbs a") %>% 
    html_text()


# price mactual, old

price_actual <- 
    SMR %>% 
    html_nodes(".sm-goods_main_details_prices_actual-price") %>% 
    html_text()
price_actual <- as.numeric( gsub("[^0-9]", "", price_actual) )


price_old <- 
    SMR %>% 
    html_nodes(".sm-goods_main_details_prices_old-price") %>% 
    html_text()
price_old <- as.numeric( gsub("[^0-9]", "", price_old) )


sizes <- 
    SMR %>% 
    html_nodes(".sm-goods_main_details_size") %>%
    html_nodes(".clearfix div") %>% 
    #html_nodes(".sm-goods__param-value") %>% 
    html_text()
sizes <- gsub("\\\n", "", sizes)

color <- 
    SMR %>%
    html_nodes (".sm-goods_main_details_color a") %>% 
    html_attr("title")




descript <- 
    SMR %>%
    html_nodes (".sm-goods__description-text") %>% 
    html_text()
descript <- gsub("\\\n", "", descript)


characteristics <- 
    SMR %>%
    html_nodes (".sm-goods_tabs_characteristix") %>% 
    #html_nodes (".characteristics_values") %>% 
    html_table ()
characteristics <- as.data.frame(characteristics[1])
