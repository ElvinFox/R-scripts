library('rvest')
library(dplyr)
library(lubridate) 
library(sjmisc)
library(gtools)
library(httr)

#install.packages(c("rvest", "dplyr", "lubridate", "sjmisc", "gtools"))


top_list <- c("https://www.lamoda.ru/c/1704/clothes-girls-niznee-belie/",
              "https://www.lamoda.ru/c/1685/clothes-girls-bluzi-i-rybawki/",
              "https://www.lamoda.ru/c/1689/clothes-girls-bruki-i-worti/",
              "https://www.lamoda.ru/c/1652/clothes-girls-verkhnyaya-odezhda/",
              "https://www.lamoda.ru/c/1665/clothes-girls-sviteri-i-kofti/",
              "https://www.lamoda.ru/c/1676/clothes-girls-dzinsi-i-denim/",
              "https://www.lamoda.ru/c/3200/clothes-komplektyikostyumy/",
              "https://www.lamoda.ru/c/5798/clothes-konverty_girls/",
              "https://www.lamoda.ru/c/1699/clothes-girls-pliaznaia-odezda/",
              "https://www.lamoda.ru/c/1675/clothes-girls-maiki-topi-tyniki/",
              "https://www.lamoda.ru/c/313/socks-nosochno-chulochnie-izdeliya/",
              "https://www.lamoda.ru/c/3052/clothes-pidgaki-kostumy-girls/",
              "https://www.lamoda.ru/c/1661/clothes-girls-platia/",
              "https://www.lamoda.ru/c/1697/clothes-girls-kostum-sportivnii/",
              "https://www.lamoda.ru/c/1671/clothes-girls-tolstovki/",
              "https://www.lamoda.ru/c/1672/clothes-girls-fytbolki-i-maiki/",
              "https://www.lamoda.ru/c/3050/clothes-shorty-girls/",
              "https://www.lamoda.ru/c/1856/clothes-girls-ubky/", 
              # girls clothes 
            
              "https://www.lamoda.ru/c/213/shoes-baletki-dlya-devochek/",
              "https://www.lamoda.ru/c/215/shoes-bosonojki-dlya-devochek/",
              "https://www.lamoda.ru/c/217/shoes-botinki-dlya-devochek/",
              "https://www.lamoda.ru/c/249/shoes-domashnyayaobuv-dlya-devochek/",
              "https://www.lamoda.ru/c/2986/shoes-krossovkiikedy/",
              "https://www.lamoda.ru/c/227/shoes-mokasiny-dlya-devochek/",
              "https://www.lamoda.ru/c/233/shoes-rezinoviesapogi-dlya-devochek/",
              "https://www.lamoda.ru/c/237/shoes-sandalii-dlya-devochek/",
              "https://www.lamoda.ru/c/229/shoes-sapogi-dlya-devochek/",
              "https://www.lamoda.ru/c/251/shoes-tufli-dlya-devochek/",
              "https://www.lamoda.ru/c/5801/shoes-pinetki_girls/",
              # girls shoes 
              
              "https://www.lamoda.ru/c/1874/default-sports-forgirls/",
              # girls sport
              
              "https://www.lamoda.ru/c/6176/default-devochkishkola/",
              # girls school
              
              "https://www.lamoda.ru/c/1626/clothes-boys-niznee-belie/",
              "https://www.lamoda.ru/c/1618/clothes-boys-bruki-i-worti/",
              "https://www.lamoda.ru/c/1591/clothes-boys-verkhnyaya-odezhda/",
              "https://www.lamoda.ru/c/1600/clothes-boys-sviteri-kardigani/",
              "https://www.lamoda.ru/c/1610/clothes-boys-dzinsi-denim/",
              "https://www.lamoda.ru/c/3078/clothes-komplekty-kostyumy-boys/",
              "https://www.lamoda.ru/c/5797/clothes-konverty_boys/",
              "https://www.lamoda.ru/c/1609/clothes-boys-maiki/",
              "https://www.lamoda.ru/c/1634/socks-boys-noski-i-kolgotki/",
              "https://www.lamoda.ru/c/3068/clothes-pidgaki-kostumy-boys/",
              "https://www.lamoda.ru/c/1732/clothes-boys-pliaznaia-odezda/",
              "https://www.lamoda.ru/c/1617/clothes-boys-rybawki/",
              "https://www.lamoda.ru/c/1625/clothes-boys-kostum-sportivnii/",
              "https://www.lamoda.ru/c/1605/clothes-boys-tolstovki/",
              "https://www.lamoda.ru/c/1606/clothes-boys-fytbolki-i-maiki/",
              "https://www.lamoda.ru/c/3066/clothes-shorty-boys/",
              # boys clothes 
              
              
              "https://www.lamoda.ru/c/255/shoes-botinki-dlya-malchikov/",
              "https://www.lamoda.ru/c/289/shoes-domashnayaobuv-dlya-malchikov/",
              "https://www.lamoda.ru/c/2988/shoes-krossovki-kedy-malchiki/",
              "https://www.lamoda.ru/c/265/shoes-mokasiny-dlya-malchikov/",
              "https://www.lamoda.ru/c/5802/shoes-pinetki_boys/",
              "https://www.lamoda.ru/c/273/shoes-rezinoviesapogi-dlya-malchikov/",
              "https://www.lamoda.ru/c/277/shoes-sandalii-dlya-malchikov/",
              "https://www.lamoda.ru/c/269/shoes-sapogi-dlya-malchikov/",
              "https://www.lamoda.ru/c/285/shoes-tufli-dlya-malchikov/",
              # boys shoes
              
              "https://www.lamoda.ru/c/6187/default-malchikishkola/",
              # boys school
              
              "https://www.lamoda.ru/c/1875/default-sports-forboys/"
              # boys sport
              
              # "https://www.lamoda.ru/c/477/clothes-muzhskaya-odezhda/?sitelink=topmenuM&l=2"
              # "https://www.lamoda.ru/c/17/shoes-men/?sitelink=topmenuM&l=3",
              # # men's clothes and shoes
              # 
              # "https://www.lamoda.ru/c/355/clothes-zhenskaya-odezhda/?sitelink=topmenuW&l=2",
              # "https://www.lamoda.ru/c/15/shoes-women/?sitelink=topmenuW&l=3"
              # # women's clothes and shoes
)

urls <- data.frame()
n <- 1

for( l in top_list){
    
    LM <- l %>% read_html()
    #LM <- "https://www.lamoda.ru/c/1875/default-sports-forboys/?page=2" %>% read_html()
    
    category <- l
    
    pages <- 
        LM %>% 
        html_nodes(".products-catalog__head-counter") %>% 
        html_text()
    pages <- ceiling( as.numeric( gsub("[^0-9]", "", pages) ) /60 )
    
    for (p in 1:pages){
        
        LM <-  paste0(l, "?page=", p) %>% read_html()

        prod_ref <- 
            LM %>% 
            html_nodes(".products-catalog__list") %>% 
            html_nodes(".products-list-item") %>% 
            html_nodes(".products-list-item__link") %>% 
            html_attr("href")
        prod_ref <-  paste0("https://www.lamoda.ru", prod_ref)

        prod_img <- 
            LM %>% 
            html_nodes(".products-catalog__list") %>% 
            html_nodes(".products-list-item")  %>% 
            html_attr("data-src")
        
        prod_img <-  paste0("http:", prod_img)
        
        url_list <- cbind( prod_ref, prod_img, category)
        url_list <- as.data.frame(url_list)
        
        urls <- rbind(urls, url_list)
        
        Sys.sleep(2)
        
        if( (p %% 3) == 0 ) Sys.sleep(round(runif(1,3,5),0))
        
        if( (p %% 8) == 0 ) Sys.sleep(round(runif(1,8,10),0))

        print(
            paste0(
                p, "/", pages, "  ", Sys.time(), "   ",  l,  
                "      sub progress -> ",  n, "/",  length(top_list)
                )
            )
    }
    Sys.sleep(10)
    n <- n + 1
}

setwd("~/R/LM")
write.csv(urls, file="front_urls.csv")





L <- read.csv("front_urls.csv", stringsAsFactors = FALSE)
lvl <- L$prod_ref

#i <- "https://www.lamoda.ru/p/re883abcajg1/shoes-reima-botinki/"

LM_df <- data.frame()

start_time <- Sys.time()
n <- 1
# SKU scrape approx 8 hrs if not paralleled


for(i in lvl) {
    LM <- i %>% read_html()
    prod_href <- i
    
    df <- data.frame()
    
    prod_brand <- 
        LM %>% 
        html_nodes(".ii-product__wrapper h1") %>% 
        html_nodes(".ii-product__brand-text") %>% 
        html_text()
    
    prod_name <- 
        LM %>% 
        html_nodes(".ii-product__wrapper") %>% 
        html_node(".heading_m.ii-product__title") %>%
        #html_node(".item-name") %>% 
        html_text()
    prod_name <- substr(prod_name, nchar(prod_brand) + 4, nchar(prod_name))

    price_new <- 
        LM %>% 
        html_nodes(".ii-product__wrapper") %>% 
        html_node(".ii-product__price.ii-product__price_several") %>% 
        html_text()
    price_new <- unlist( strsplit(price_new,"%", fixed = TRUE))
    price_new <- as.numeric( gsub("[^0-9]", "", price_new[1]) )
    
    price_old <- 
        LM %>% 
        html_nodes(".ii-product__wrapper") %>% 
        html_node(".ii-product__price.ii-product__price_several") %>% 
        html_text()
    price_old <- unlist( strsplit(price_old,"%", fixed = TRUE))
    price_old <- as.numeric( gsub("[^0-9]", "", price_old[2]) )
    
    df <- cbind(prod_name, prod_brand, prod_href, price_new, price_old)

    tree <-
        LM %>%
        html_node(".ii-product__wrapper") %>% 
        html_nodes(".breadcrumbs span") %>% 
        html_nodes(".js-breadcrumbs__item-text") %>%
        #html_nodes(".breadcrumbs__item") %>%
        html_text()
    tree <- tree[-1]
    
    
    for(u in 1:length(tree) ){
        df <-  cbind (df, assign(paste("lvl", u, sep = ""), tree[u]))
        colnames(df)[ncol(df)]<- paste0("lvl_",u)
    }
    
    df <- as.data.frame(df)
    
    
    characteristics_1 <- 
        LM %>% 
        html_node(".ii-product__description-text") %>% 
        html_nodes(".ii-product__attribute-label") %>% 
        html_text()
    characteristics_1 <- gsub("[^À-ÿ ]", "", characteristics_1)

##################    
    characteristics_2 <- 
        LM %>% 
        html_node(".ii-product__description-text") %>% 
        html_nodes(".ii-product__attribute-value") %>% 
        html_text()
    characteristics_2 <- gsub("[^À-ÿ ]", "", characteristics_2)
    
    chrs <- cbind(characteristics_1, characteristics_2)
    chrs <- as.data.frame(chrs)
    
    
    for(p in 1:nrow(chrs) ){
        df <-  cbind (df, assign(paste("Desc", p, sep = ""), chrs[p,2]))
        colnames(df)[ncol(df)]<- as.character(chrs[p,1])
    }
    
    LM_df <- smartbind(LM_df,df)
    
    now <- Sys.time()
    tm <- round(difftime(now, start_time, units = "mins"),2)
    
    Sys.sleep(1)
    
    if ((n %% 10) == 0)
        cat(
            paste0("\n\ni -> ", n, " / ",length(lvl),"  \n",
                   "Time passed: ", round(difftime(now, start_time, units = "mins"),2), " mins", "  \n",
                   "Estimated time left: ", round( (length(lvl)-n) / ( n/as.numeric(tm) ),2) , " mins",
                   " / Hours -> ",round( (length(lvl)-n) / ( n/as.numeric(tm) ) /60, 2)
            )
        )
   
    if ((n %% 5) == 0)
        Sys.sleep(round(runif(1, 2, 10), 0))
    
    n <- n+1
}

write.csv(LM_df, "LM_df.csv")


