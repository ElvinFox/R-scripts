library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)
library(gtools)

top_list <- c("https://www.rendez-vous.ru/catalog/girls/",
              "https://www.rendez-vous.ru/catalog/boys/",
              "https://www.rendez-vous.ru/catalog/female/",
              "https://www.rendez-vous.ru/catalog/male/")

urls <- data.frame()

for( l in top_list){
    
    RND <- l %>% read_html()
    #RND <- "https://www.rendez-vous.ru/catalog/female/" %>% read_html()
    
    pages <- 
        RND %>% 
        html_nodes(".pagination.pagination-top li a") %>% 
        html_text() %>%  as.numeric()
    pages <- max( pages)
    
    for (p in 1:pages){

            RND <-  paste0(l, "page/", p, "/") %>% 
            read_html()
        
         

        prod_ref <- 
            RND %>% 
            html_nodes(".list-items.list-items-catalog li a") %>% 
            html_attr("href")
        prod_ref <-  paste0("https://www.rendez-vous.ru", prod_ref)
        
        prod_img <- 
            RND %>% 
            html_nodes(".list-items.list-items-catalog li a") %>%
            html_node(".item-image img") %>% 
            html_attr("data-srcset")
        prod_img <- gsub(" .*","", prod_img)

        url_list <- cbind( prod_ref, prod_img)
        url_list <- as.data.frame(url_list)
        
        urls <- rbind(urls, url_list)
        
        if( (p %% 5) == 0 ) Sys.sleep(round(runif(1,1,2),0))
        
        print(paste0(p,"/", pages, "  ", Sys.time(), "   ", l))
    }
}

setwd("~/RND")
write.csv(urls, file="front_urls.csv")

L <- read.csv("front_urls.csv", stringsAsFactors = FALSE)

#lvl <- as.character(urls$prod_ref)

lvl <- L$prod_ref

#i <- "https://www.rendez-vous.ru/catalog/female/kedy/lacoste_cfa0066_ziane_bl_2_belyy-2123916/"

RND_df <- data.frame()

start_time <- Sys.time()
n <- 1
# SKU scrape approx 8 hrs if not paralleled
# timed out at 2.4k after 49 min 

for(i in lvl) {
    RND <- i %>% read_html()
    prod_href <- i
    
    df <- data.frame()
    
    prod_name <- 
        RND %>% 
        html_nodes(".item-details") %>% 
        html_node(".item-name") %>% 
        html_text()
    prod_name <- gsub("\\\n        ", "", prod_name)
    
    prod_brand <- 
        RND %>% 
        html_nodes(".item-details") %>% 
        html_node(".item-name a") %>% 
        html_text()
    
    price_new <- 
        RND %>% 
        html_node(".item-info") %>% 
        html_node(".item-price-new span") %>% 
        html_text()
    price_new <- as.numeric( gsub("[^0-9]", "", price_new) )
    
    
    price_old <- 
        RND %>%
        html_node(".item-info") %>% 
        html_node(".item-price-old") %>% 
        html_text()
    price_old <- as.numeric( gsub("[^0-9]", "", price_old) )
    
    df <- cbind(prod_name, prod_brand, prod_href, price_new, price_old)
    
    
    tree <-
        RND %>%
        html_nodes(".breadcrumbs ul li a") %>% 
        #html_nodes(".breadcrumbs-wrapper") %>%
        #html_nodes(".breadcrumbs__item") %>%
        html_text()
    tree <- tree[-1]
    
    
    for(u in 1:length(tree) ){
        df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
        colnames(df)[ncol(df)]<- paste0("lvl_",u)
    }
    
    df <- as.data.frame(df)
    
    
    characteristics_1 <- 
        RND %>% 
        html_node(".flex-grid-row") %>% 
        html_nodes(".flex-col-1") %>% 
        html_nodes(".data-title") %>% 
        html_text()
    
    characteristics_2 <- 
        RND %>% 
        html_node(".flex-grid-row") %>% 
        html_nodes(".flex-col-1") %>% 
        html_nodes(".table-of-data dd") %>% 
        html_text()
    
    chrs <- cbind(characteristics_1, characteristics_2)
    chrs <- as.data.frame(chrs)
    
    
    for(p in 1:nrow(chrs) ){
        df <-  cbind (df, assign(paste("Desc", p, sep = ""), chrs[p,2]))
        colnames(df)[ncol(df)]<- as.character(chrs[p,1])
    }
    
    RND_df <- smartbind(RND_df,df)
    
    now <- Sys.time()
    tm <- round(difftime(now, start_time, units = "mins"),2)
    
    if ((n %% 10) == 0)
        cat(
            paste0("\n\ni -> ", n, " / ",length(lvl),"  \n",
                   "Time passed: ", round(difftime(now, start_time, units = "mins"),2), " mins", "  \n",
                   "Estimated time left: ", round( (length(lvl)-n) / ( n/as.numeric(tm) ),2) , " mins",
                   " / Hours -> ",round( (length(lvl)-n) / ( n/as.numeric(tm) ) /60, 2)
            )
        )
    if ((n %% 5) == 0)
        Sys.sleep(1)
    
    if ((n %% 50) == 0)
        Sys.sleep(round(runif(1, 3, 5), 0))
    
    n <- n+1
}

write.csv(RND_df, "RND_df.csv")
