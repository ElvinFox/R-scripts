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
# ?pageSize=120


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
