library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
library(sjmisc)
library(gtools)
library(stringr)


# most probably Rselenium is needed  [JAVA] is required for work

### https://www.ozon.ru/category/7500/
### https://www.ozon.ru/category/11424/ 
    
# 28 SKU per page
# not clear how the site deals with ?pages


input <- "https://www.ozon.ru/category/7500/"
url <- "https://www.ozon.ru/category/7500/"


url <- paste0(input, "?page=",2 )
OZN <- url %>% read_html()

as.character(OZN)

OZN %>% 
    html_nodes(".layout-page")



document.querySelector('body > iframe:nth-child(1)')

nmbr <- 
    OZN %>% 
    html_nodes(".category-count") %>% 
    html_text()
gsub("[^0-9]", "", nmbr)

# find depth for scrapping
max_pages <- nmbr/28

prod <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_nodes(".tile") %>% 
    html_text()


prod_img <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_nodes(".tile") %>% 
    html_nodes(".img-inner-wrapper") %>% 
    html_text()

prod_price <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_nodes(".price-container") %>% 
    html_nodes(".img-inner-wrapper") %>% 
    html_text()

prod_id <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_attr(".id") %>% 
    html_text()

prod_id <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_attr(".id") %>% 
    html_text()


prod_name <- 
    OZN %>% 
    html_nodes(".tiles") %>% 
    html_attr(".name") %>% 
    html_text()
