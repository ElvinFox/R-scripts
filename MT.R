library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
library(gtools) 

input <- "https://www.mytoys.ru/%D0%9E%D0%B4%D0%B5%D0%B6%D0%B4%D0%B0/KID/ru-mt.cw/"
 

# 48 products per page
last_page <- 462

#last_page <- 20
start_time <- Sys.time()
Parsed <- list()

for( i in 452:last_page ) {
    url <- paste0(input, "?page=", i)
    MT <- url %>% read_html()

    prod_id <- 
        MT %>%
        html_nodes(".gf_tab_content") %>% 
        html_nodes(".catBox") %>% 
        html_nodes(".prod_item") %>% 
        html_attr("data-product-id")
    
    prod_img_link <- 
        MT %>%
        html_nodes(".gf_tab_content") %>% 
        html_nodes(".catBox") %>% 
        html_nodes(".prod_item") %>% 
        html_nodes(".sku_image") %>% 
        html_nodes(".ddl_product_link img") %>% 
        html_attr("src")
    
    parsed <- data.frame(
        prod_id, prod_img_link, stringsAsFactors = FALSE)
    
    Parsed <- rbind(parsed, Parsed)
    now <- Sys.time()
    
    if ((i %% 5) == 0)
        cat(
            paste0("\n i -> ", i, "  \n",
                   "Time passed: ", round(difftime(now, start_time, units = "mins"),2), " mins" )
        )
}

new_ids <- ids[1628:length(ids)]
length(ids)

ids <- as.numeric(Parsed$prod_id)

###### parsing by SKU ###############
write.csv2(Parsed, file = "Parsed.csv")
ids <- read.csv2("Parsed.csv")
ids <- ids[,2]

mega_df <- data.frame()

begin <- Sys.time()
n <- 1628

input <- "https://www.mytoys.ru/product/"

for (i in new_ids) {
    
    url <- paste0(input, i, "?sku=", i)
    
    mod2 = try( MT <- url %>% read_html() , TRUE)
    if (isTRUE(class(mod2) == "try-error")) {
            Sys.sleep(round(runif(1, 10, 12), 0))
            next
    } else {
            MT <- url %>% read_html()
            
            prod_name <- 
                    MT %>%
                    html_nodes(".grid_5 h1") %>% 
                    html_text()
            
            brand <- 
                    MT %>%
                    html_nodes(".pdp__madeby a") %>% 
                    html_text()
            
            id <- i
            
            
            price_normal <- 
                    MT %>%
                    html_nodes(".price--normal") %>% 
                    html_text()
            if(is_empty(price_normal)==TRUE) price_normal <- "NA"
            price_normal <- as.numeric(gsub("[^0-9]", "", price_normal))  
            
            price_new <- 
                    MT %>%
                    html_nodes(".price--reduced") %>% 
                    html_text()
            if(is_empty(price_new)==TRUE) price_new <- "NA"
            price_new <- as.numeric(gsub("[^0-9]", "", price_new))  
            
            price_old <- 
                    MT %>%
                    html_nodes(".price--canceled") %>% 
                    html_text() 
            if(is_empty(price_old)==TRUE) price_old <- "NA"    
            price_old <- as.numeric(gsub("[^0-9]", "", price_old))   
            
            
            
            for_ages <- 
                    MT %>%
                    html_nodes(".infoArea div ul li") %>% 
                    html_text()
            for_ages <- for_ages[1]
            
            
            desc <- 
                    MT %>%
                    html_nodes(".infoArea div") 
            desc <- desc[1]
            desc <- as.character(desc)
            D <- strsplit(desc, "<br>")
            D <- unlist(D)
            D <- gsub(".*•" ,"",D)
            D <- gsub("<p>.*" ,"",D)
            D
            
            df <- cbind(id, prod_name, brand, for_ages, price_normal, price_new, price_old)
            
            
            for(i in 1:length(D) ){
                    df <-  cbind (df, assign(paste("Var", i, sep = ""), D[i]))
                    colnames(df)[ncol(df)]<- paste0("Desc_",i)
                    
            }
            
            type <- 
                    MT %>%
                    html_nodes(".breadcrumbs__item") %>% 
                    html_text()
            type <- type[2 : length(type)]
            
            for(i in 1:length(type) ){
                    df <-  cbind (df, assign(paste("lvl", i, sep = ""), type[i]))
                    colnames(df)[ncol(df)]<- paste0("lvl_",i)
            }
    }
    
    
    now <- Sys.time()
    
    if ((n %% 5) == 0)
        Sys.sleep(round(runif(1, 1, 2), 0))
    
    if ((n %% 10) == 0)
        cat(
            paste0("\n i -> ", n, "  \n",
                   "Time passed: ", round(difftime(now, begin, units = "mins"),2), " mins" )
        )

    n <- n + 1
    
    df <- as.data.frame(df)
    
    mega_df <- smartbind(mega_df,df)
}




write.csv2(mega_df, file = "MT_parsed.csv")











## save images  of SKUs

# y = prod_img_link[1]
# download.file(y, 'y.jpg', mode = 'wb')


# product page is formed as follows:
# https://www.mytoys.ru/product/XXX?sku=XXX   where XXX ==  prod_id



