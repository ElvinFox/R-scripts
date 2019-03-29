library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
library(sjmisc)
library(gtools)




#### part that collects infor from the main page
#### collecting info about img link, id, SKU link, prod category 

input <- "https://www.bebakids.ru/dlya-detei/odezhda-dlya-podrostkov/"
BBK <- input %>% read_html()

last_page <-     
    BBK %>%
    html_nodes(".paginator_wrap a") %>% 
    html_attr("title")

last_page <- max( as.numeric( gsub("[^0-9]", "", last_page) ) , na.rm = TRUE)

front <- data.frame()
begin <- Sys.time()

for(i in 1:last_page){
    
    url <- paste0(input, i, "?pagen_2=", i)
    
    BBK <- url %>% read_html()
    
    prod_img <- 
        BBK %>%
        html_nodes(".cat_img")
    prod_img <-  gsub(".*src" ,"", prod_img)
    prod_img <-  gsub("width.*" ,"", prod_img)
    prod_img <-  gsub(".*upload" ,"", prod_img)
    prod_img <-  substr(prod_img, 1, nchar(prod_img) -2)
    prod_img <- paste0("https://www.bebakids.ru/upload",  prod_img)
    
    prod_link <- 
        BBK %>%
        html_nodes(".cat_li_1 a") 
    prod_link <-   gsub("class.*" , "", prod_link) 
    prod_link <-  paste0("https://www.bebakids.ru/", substr(prod_link, 11, nchar(prod_link) -2))
    
    
    prod_id <-
        BBK %>%
        html_nodes(".cat_li_1") %>% 
        html_attr("data-id") %>% 
        as.numeric()
    
    
    
    prod_category <-
        BBK %>%
        html_nodes(".cat_brand_name") %>% 
        html_nodes(".cat_name") %>% 
        html_text()
    
    
    now <- Sys.time()
    
    if ((i %% 5) == 0)
        Sys.sleep(round(runif(1, 1, 2), 0))
    
    if ((i %% 10) == 0)
        cat(
            paste0("\n i -> ", i, "  \n",
                   "Time passed: ", round(difftime(now, begin, units = "mins"),2), " mins" )
        )
    
    
    bf <- cbind(prod_id, prod_img, prod_category, prod_link)
    bf <- as.data.frame(bf)
    
    front <- smartbind(front, bf)
}


write.csv2(front, "BBK_front.csv")
  








####  Parsing by SKU  
#links <- as.character( front$prod_link )
test <- read.csv2("BBK_front.csv", header = TRUE, stringsAsFactors = FALSE)
links <- test$prod_link
start_time <- Sys.time()
n <- 1
BBK_df <- data.frame()

for (i in links){

    X = try( BBK <- i %>% read_html() , TRUE)
    if (isTRUE(class(X) == "try-error")) {
        Sys.sleep(round(runif(1, 10, 12), 0))
        next
    } else {
        BBK <- i %>% read_html()
        
        price_new <- 
            BBK %>% 
            html_nodes(".wr_item_3_price") %>% 
            html_text()
        price_new <- as.numeric( gsub("[^0-9]", "", price_new[1]) )
        
        price_old <- 
            BBK %>% 
            html_nodes(".wr_item_3_old_price") %>% 
            html_text()
        price_old <- as.numeric( gsub("[^0-9]", "", price_old[1]) )
        
        prod_title <- 
            BBK %>% 
            html_nodes(".wr_item_2_1 h1") %>% 
            html_text()
        
        df <- cbind(prod_title, price_new, price_old, i)
        
        ### table of description
        des <- 
            BBK %>% 
            html_nodes(".wr_item_2_2 div") %>% 
            html_text()
        
        des <- gsub("[\t \n]", "", des)
        
        for(p in 1:length(des) ){
            df <-  cbind (df, assign(paste("Desc", p, sep = ""), des[p]))
            colnames(df)[ncol(df)]<- paste0("Desc_",p)
        }
        
        ### product tree
        tree <-
            BBK %>%
            html_nodes(".wr_cont_0 ul li") %>% 
            html_text()
        tree <- tree[-1]
        tree <- gsub("/","", tree, fixed = TRUE)
        
        for(u in 1:length(tree) ){
            df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
            colnames(df)[ncol(df)]<- paste0("lvl_",u)
        }
    }
    
    now <- Sys.time()
    
    tm <- round(difftime(now, start_time, units = "mins"),2)

    if ((n %% 10) == 0)
        cat(
            paste0("\n\ni -> ", n, "  \n",
                   "Time passed: ", round(difftime(now, start_time, units = "mins"),2), " mins", "  \n",
                   "Estimated minutes left: ", round( (length(links)-n) / ( n/as.numeric(tm) ),2) , 
                   " / Hours -> ",round( (length(links)-n) / ( n/as.numeric(tm) ) /60, 2)
                   )
        )
    if ((n %% 5) == 0)
        Sys.sleep(round(runif(1, 1, 2), 0))
    n <- n + 1
    
    df <- as.data.frame(df)
    
    BBK_df <- smartbind(BBK_df,df)
}


setwd("~/BBK")
write.csv2(BBK_df, "BBK_df.csv")
setwd("~/")





tm <- round(difftime(now, start_time, units = "mins"),2)

n/as.numeric(tm) # per minute
(length(links)-n) / ( n/as.numeric(tm) ) # time left in minutes
(length(links)-n) / ( n/as.numeric(tm) ) /60 # time left in hours
