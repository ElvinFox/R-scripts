library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)
library(gtools)



start_url <- c("https://www.dochkisinochki.ru/icatalog/categories/odezhda/",
"https://www.dochkisinochki.ru/icatalog/categories/obuv/",
"https://www.dochkisinochki.ru/icatalog/categories/shkolnaya-odezhda/")


DS <- "https://www.dochkisinochki.ru/icatalog/categories/odezhda/" %>% read_html()

page <- DS %>% 
    html_nodes(".catMsbar") %>%
    html_nodes(".pagination-new a") %>% html_text() %>% as.character()

last_page <- max (as.numeric( gsub("[^0-9]", "", page) ) , na.rm = TRUE)
    

Ids <- list()

for (i in 1:last_page) {
    input <-
        'https://www.dochkisinochki.ru/icatalog/categories/odezhda/'  # <- starting link
    
    url <- paste0(input, '?line=&PAGEN_1=', i)
    DS <- url %>% read_html()
    
    
    # id of SKUs
    href <-
        DS %>%
        html_nodes(".esk-content") %>% 
        html_nodes(".jDataLink") %>% html_attr("href")
    
    
    Ids <- rbind(Ids, href)
    
    print(paste0("i = ", i, " ", Sys.time()))
    if( (i %% 5) == 0 ) Sys.sleep(round(runif(1,1,2),0))
}



lvl <- Ids

DS_df <- data.frame()
start_time <- Sys.time()

n <- 1
for (i in lvl) {
    
    input <-'https://www.dochkisinochki.ru/icatalog/products/'  # <- starting link
    
    url <- paste0(input,  i, "/")
    
    
    mod2 = try( DS <- url %>% read_html() , TRUE)
    if (isTRUE(class(mod2) == "try-error")) {
        next
    } else {
        DS <- url %>% read_html()
        
        img_link <- DS %>%
            html_node(".cloudzoom-gallery")
        img_link <- gsub(".*zoomImage:.", "", img_link)
        img_link <- substr(img_link, 1, nchar(img_link) - 4)
        
        
        ### product tree, it has main part and sub part (tree_2)
        tree_1 <-
            DS %>%
            html_nodes(".shop-center-module") %>%
            html_nodes(".breadcrumb") %>% 
            html_nodes(".ulpopup a") %>% html_text()
        
        tree_1 <- as.data.frame(tree_1)
        colnames(tree_1)<- "col1"
        
        
        tree_2 <-
            DS %>%
            html_nodes(".shop-center-module") %>%
            html_nodes(".breadcrumb") %>% 
            html_nodes(".nopopup a") %>% html_text()

        tree <- rbind(tree_1, tree_2)
        
        for(u in 1:nrow(tree) ){
            df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u,1]))
            colnames(df)[ncol(df)]<- paste0("lvl_",u)
        }
        
        
        # characteristics
        tbl <- DS %>%
            html_nodes(".tth_text table") %>%
            html_table()
        tbl <- as.data.frame(tbl)
        
        df <- cbind(img_link)
        
        
        for(p in 1:nrow(tbl) ){
            df <-  cbind (df, assign(paste("Desc", p, sep = ""), tbl[p,2]))
            colnames(df)[ncol(df)]<- tbl[p,1]
        }

    }
    df <- as.data.frame(df)
    
    DS_df <- smartbind(DS_df,df)
    
    tm <- round(difftime(now, start_time, units = "mins"),2)
    
    now <- Sys.time()
    
    passed <- difftime(now, start_time, units = 'mins')
    
    if ((n %% 50) == 0)
    cat(
        paste0("\n\ni -> ", n, "  \n",
               "Time passed: ", round(difftime(now, start_time, units = "mins"),2), " mins", "  \n",
               "Estimated minutes left: ", round( (length(lvl)-n) / ( n/as.numeric(tm) ),2) , 
               " / Hours -> ",round( (length(lvl)-n) / ( n/as.numeric(tm) ) /60, 2)
        )
    )
    if ((n %% 6) == 0)
        Sys.sleep(round(runif(1, 1, 2), 0))
    n <- n + 1
}

Tbl_details <- as.data.frame(df)


write.csv2(Tbl_details, "DS_data.csv")
