library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)
library(gtools)

top_list <- c("https://sport-marafon.ru/catalog/odezhda/", "https://sport-marafon.ru/catalog/obuv/" )

urls <- data.frame()

for( l in top_list){
    
    SMF <- l %>% read_html()

    pages <- 
        SMF %>% 
        html_nodes(".navigate li") %>% html_text()
    pages <- max( as.numeric(gsub("[^0-9]", "", pages)), na.rm = TRUE)
    
    for (p in 1:pages){
        
        SMF <-  paste0(l, "?PAGEN_1=", p ,"&sort=sort&order=desc")%>% 
            read_html()
        
        prod_ref <- 
            SMF %>% 
            html_nodes(".product-list") %>%  
            html_nodes(".product-list__item ") %>%  
            html_attr("href")
        prod_ref <-  paste0("https://sport-marafon.ru", prod_ref)
        
        prod_img <- 
            SMF %>% 
            html_nodes(".product-list__item ") %>% 
            html_nodes(".product-list__image") %>% 
            html_attr("src")
        prod_img <- paste0("https://sport-marafon.ru", prod_img)
        
        url_list <- cbind( prod_ref, prod_img)
        url_list <- as.data.frame(url_list)
        
        urls <- rbind(urls, url_list)
        
        print(paste0(p,"/", pages, "  ", Sys.time()))
    }
}



lvl <- urls$prod_ref



SMF_df <- data.frame()

n <- 1
# SKU scrape

for(i in lvl) {
    SMF <- i %>% read_html()
    prod_href <- i
    
    df <- data.frame()
    
    prod_name <- 
        SMF %>% 
        html_nodes(".catalog-detail__name") %>% 
        html_text()
    
    
    price_new <- 
        SMF %>% 
        html_nodes(".catalog-detail__price.catalog-detail__price_new") %>% 
        html_text()
    
    price_new <- as.numeric( gsub("[^0-9]", "", price_new) )
    
    
    price_old <- 
        SMF %>% 
        html_nodes(".catalog-detail__price.catalog-detail__price_old") %>% 
        html_text()
    
    price_old <- as.numeric( gsub("[^0-9]", "", price_old) )
    
    price_act <- 
        SMF %>% 
        html_nodes(".catalog-detail__price") %>% 
        html_text()
    
    price_act <- as.numeric( gsub("[^0-9]", "", price_act) )
    
    
    df <- cbind(prod_name, prod_href, price_new, price_old, price_act)
    
    tree <-
        SMF %>%
        html_nodes(".wrapper") %>% 
        html_nodes(".breadcrumbs-wrapper") %>%
        html_nodes(".breadcrumbs__item") %>%
        html_text()
    
    
    for(u in 1:length(tree) ){
        df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
        colnames(df)[ncol(df)]<- paste0("lvl_",u)
    }
    
    df <- as.data.frame(df)
    
    
    characteristics_1 <- 
        SMF %>% 
        html_node(".characteristic table") %>% 
        html_nodes(".characteristic__name.d-inline.d-lg-none") %>% 
        html_text()
    
    characteristics_2 <- 
        SMF %>% 
        html_node(".characteristic table") %>% 
        html_nodes(".characteristic__value") %>% 
        html_text()
    
    chrs <- cbind(characteristics_1, characteristics_2)
    chrs <- as.data.frame(chrs)
    
    
    for(p in 1:nrow(chrs) ){
        df <-  cbind (df, assign(paste("Desc", p, sep = ""), chrs[p,2]))
        colnames(df)[ncol(df)]<- as.character(chrs[p,1])
    }
    
    
    SMF_df <- smartbind(SMF_df,df)
    
    print(paste0(n,"/", length(lvl), "  ", Sys.time()))
    n <- n+1
}
