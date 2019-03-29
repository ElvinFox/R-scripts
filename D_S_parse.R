library('rvest')
library(dplyr)
library(tibble)
library(lubridate) 
#library(imager)
library(sjmisc)






Seasons <- list()
Collections <- list()
Genders <- list()
Ages <- list()
IDs <- list()
Img_links <- list()
Linings <- list()
Styles <- list()
Heights <- list()
Compositions <- list()
Materials <- list()
Water_resists <- list()
Air_permeabilitys <- list()
Outsole_materials <- list()
Collection_years <- list()

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
                # characteristics
                tbl <- DS %>%
                        html_nodes(".tth_text table") %>%
                        html_table()
                tbl <- as.data.frame(tbl)
                
                img_link <- DS %>%
                        html_node(".cloudzoom-gallery")
                img_link <- gsub(".*zoomImage:.", "", img_link)
                img_link <- substr(img_link, 1, nchar(img_link) - 4)
                
                Season <- ifelse(is_empty(tbl[grepl("Сезон", tbl$X1), 2]), "NA",tbl[tbl$X1 == "Сезон", 2])
                Style <- ifelse(is_empty(tbl[grepl("Назначение", tbl$X1), 2]), "NA" ,tbl[tbl$X1 == "Назначение", 2])
                Height <- ifelse(is_empty(tbl[grepl("Рост", tbl$X1), 2]), "NA",tbl[tbl$X1 == "Рост", 2])
                Composition <- ifelse(is_empty(tbl[grepl("Состав", tbl$X1), 2]), "NA", tbl[grepl("Состав", tbl$X1), 2])
                Material <- ifelse(is_empty(tbl[grepl("^Материал$", tbl$X1), 2]), "NA", tbl[grepl("^Материал$", tbl$X1), 2])
                Water_resist <- ifelse(is_empty(tbl[grepl("Водонепроницаемость", tbl$X1), 2]), "NA", tbl[grepl("Водонепроницаемость", tbl$X1), 2])
                Air_permeability <- ifelse(is_empty(tbl[grepl("Воздухопроницаемость", tbl$X1), 2]), "NA", tbl[grepl("Воздухопроницаемость", tbl$X1), 2])
                Outsole_material <- ifelse(is_empty(tbl[grepl("Материал подошвы", tbl$X1), 2]), "NA", tbl[grepl("Материал подошвы", tbl$X1), 2])   
                Lining <- ifelse(is_empty(tbl[grepl("Подкладка", tbl$X1), 2]), "NA", tbl[grepl("Подкладка", tbl$X1), 2])
                Collection <- ifelse(is_empty(tbl[grepl("Коллекция", tbl$X1), 2]), "NA", tbl[grepl("Коллекция", tbl$X1), 2])
                Collection_year <- ifelse(is_empty(tbl[grepl("Год коллекции", tbl$X1), 2]), "NA", tbl[grepl("Год коллекции", tbl$X1), 2])
                Gender <- ifelse(is_empty(tbl[grepl("Пол", tbl$X1), 2]), "NA", tbl[grepl("Пол", tbl$X1), 2])
                Age <- ifelse(is_empty(tbl[grepl("Возраст", tbl$X1), 2]), "NA", tbl[grepl("Возраст", tbl$X1), 2])
                
                
                Seasons <- rbind(Seasons, Season)
                Collections <- rbind(Collections, Collection)
                Genders <- rbind(Genders, Gender)
                Ages <- rbind(Ages, Age)
                IDs <- rbind(IDs, i)
                Img_links <- rbind(Img_links, img_link)
                Linings <- rbind(Linings, Lining)
                Styles <- rbind(Styles,Style)
                Heights <- rbind(Heights,Height)
                Compositions <- rbind(Compositions, Composition)
                Materials <- rbind(Materials, Material)
                Water_resists <- rbind(Water_resists,Water_resist)
                Air_permeabilitys <- rbind(Air_permeabilitys,Air_permeability)
                Outsole_materials <- rbind(Outsole_materials,Outsole_material)
                Collection_years <- rbind(Collection_years,Collection_year)
        }
        
        
        
        now <- Sys.time()
        
        passed <- difftime(now, start_time, units = 'mins')
        
        if ((n %% 50) == 0)
                print(paste0("i = ", n, " minutes passed: ", passed))
        if ((n %% 6) == 0)
                Sys.sleep(round(runif(1, 1, 2), 0))
        n <- n + 1
}

Seasons <- unlist(Seasons)
Collections <- unlist(Collections)
Genders <- unlist(Genders)
Ages <- unlist(Ages)
IDs <- unlist(IDs)
Img_links <- unlist(Seasons)
Linings <- unlist(Linings)
Styles <- unlist(Styles)
Heights <- unlist(Heights)
Compositions <- unlist(Compositions)
Materials <- unlist(Materials)
Water_resists <- unlist(Water_resists)
Air_permeabilitys <- unlist(Air_permeabilitys)
Outsole_materials <- unlist(Outsole_materials)
Collection_years <- unlist(Collection_years)



Tbl_details <- data.frame(
        Seasons, Collections, Genders, Ages, IDs, Img_links, 
        Linings, Styles, Heights, Compositions, Materials, 
        Water_resists, Air_permeabilitys, Outsole_materials, 
        Collection_years, stringsAsFactors = FALSE
)


write.csv2(Tbl_details, "DS_data.csv")




##   9678567 <- error



for(i in nmbrs) {
        
        input <-'https://www.dochkisinochki.ru/icatalog/products/'  # <- starting link
        
        url <- paste0(input,  i, "/")
        
        
        mod2 = try( DS <- url %>% read_html() , TRUE)
        if (isTRUE(class(mod2) == "try-error")) {
                print("error")
                next
        } else {
                DS <- url %>% read_html()
                print("ok")
        }
}



nmbrs <- c(9455103,10266419,9678567,9920634)
