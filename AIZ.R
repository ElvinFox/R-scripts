library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)
library(gtools)



top_list <- c("https://aizel.ru/detskoe/devochki/", 
              "https://aizel.ru/detskoe/malchiki/")

urls <- data.frame()

for( l in top_list){
    
    AIZ <- l %>% read_html()
    #AIZ <- "https://aizel.ru/detskoe/devochki/" %>% read_html()
    
    pages <- 
        AIZ %>% 
        html_nodes(".pagination li a") %>% 
        html_text() %>%  as.numeric()
    pages <- max( pages)
    
    for (p in 1:pages){
        
        AIZ <-  paste0(l, "?page=", p) %>% 
            read_html()
        

        
        prod_ref <- 
            AIZ %>% 
            html_nodes(".product__list li") %>% 
            html_node(".product__slider a") %>% 
            html_attr("href")
        prod_ref <-  paste0("https://aizel.ru", prod_ref)

        prod_img <- 
            AIZ %>% 
            html_nodes(".product__list li") %>% 
            html_node(".product__slider a img") %>% 
            html_attr("src")
        prod_img <- paste0("https:", prod_img)

        
        url_list <- cbind( prod_ref, prod_img)
        url_list <- as.data.frame(url_list)
        
        urls <- rbind(urls, url_list)
        
        #Sys.sleep(3)  not needed
        
        print(paste0(p,"/", pages, "  ", Sys.time(), "   ", l))
    }
}

setwd("~/AIZ")
write.csv(urls, file="front_urls.csv")




# scrape by SKU
# total time ~ 1-2 hrs if not paralleled

L <- read.csv("front_urls.csv", stringsAsFactors = FALSE)

#lvl <- as.character(urls$prod_ref)

lvl <- L$prod_ref

#i <- "https://aizel.ru/bubbles/bezheviy-kombinezon-iz-hlopka-72189/"

AIZ_df <- data.frame()

start_time <- Sys.time()
n <- 1
# SKU scrape

for(i in lvl) {
    AIZ <- i %>% read_html()
    prod_href <- i
    
    df <- data.frame()
    
    prod_name <- 
        AIZ %>% 
        html_nodes(".product__wrapper span") %>% 
        html_text()
    prod_name <- prod_name[1]
    
    prod_brand <- 
        AIZ %>% 
        html_nodes(".product__wrapper a") %>% 
        html_text()
    
    
    price <- 
        AIZ %>% 
        html_nodes(".product-item__name-desc span") %>%
        html_attr("content") %>% 
        as.numeric()
    
    price <- price[1]
        
    price_new <- 
        AIZ %>% 
        html_nodes(".product__wrapper") %>% 
        html_node(".product-item__price.product-item__price_discount") %>% 
        html_attr("content") %>% 
        as.numeric()
    
    
    price_old <- 
        AIZ %>% 
        html_nodes(".product__wrapper") %>% 
        html_node(".product-item__price.product-item__price_old") %>% 
        html_attr("content") %>% 
        as.numeric()
    
    df <- cbind(prod_name, prod_brand, prod_href, price, price_new, price_old)
    
    
    # ## NOT DONE ###
    # 
    # 
    # tree <-
    #     AIZ %>%
    #     html_nodes(".breadcrumbs ul li a") %>%
    #     #html_nodes(".breadcrumbs-wrapper") %>%
    #     #html_nodes(".breadcrumbs__item") %>%
    #     html_text()
    # tree <- tree[-1]
    # 
    # 
    # for(u in 1:length(tree) ){
    #     df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
    #     colnames(df)[ncol(df)]<- paste0("lvl_",u)
    # }

    df <- as.data.frame(df)
    
    
    
    
    characteristics_1 <- 
        AIZ %>% 
        html_node(".accordion__item__content") %>% 
        html_nodes(".details__row span") %>% 
        html_text()
    
    characteristics_2 <- 
        AIZ %>% 
        html_node(".accordion__item__content") %>% 
        html_nodes(".details__row")
    
    characteristics_2 <- gsub(".*/span", "", characteristics_2)
    characteristics_2 <- gsub("<.*", "", characteristics_2)
    characteristics_2 <- gsub("                    ", "", characteristics_2)
    characteristics_2 <- substr(characteristics_2, 5, nchar(characteristics_2))
    
    
    chrs <- cbind(characteristics_1, characteristics_2)
    chrs <- as.data.frame(chrs)
    
    if ( (is_empty(characteristics_1) & is_empty(characteristics_2)) == FALSE  ){
        for(p in 1:nrow(chrs) ){
            df <-  cbind (df, assign(paste("Desc", p, sep = ""), chrs[p,2]))
            colnames(df)[ncol(df)]<- as.character(chrs[p,1])
        }
    }

    
    AIZ_df <- smartbind(AIZ_df,df)
    
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
        Sys.sleep(round(runif(1, 1, 3), 0))
    
    n <- n+1
}

write.csv(AIZ_df, "AIZ_df.csv")

