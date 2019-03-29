library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)  # should avoid using with dplyr otherwise summarise() won't work correctly
# but it is useful for labels centering
library(scales)
library(dplyr)



df <- read.csv2("C:/Users/diu/Desktop/BP.csv", header =TRUE)
df <- read.csv2("C:/Users/diu/Desktop/Raw_data.csv", header =TRUE)

df <- df[!is.na(df$кол.во.sku),]
df <- df[,-1]

mf <- df %>% 
    filter(пол == "boy") %>% 
    #select(канал.продаж,пол, стиль, сезон.продаж, группа.видов.ассортимента, вид.асс.та, кол.во.sku) %>% 
     
    #head(5) %>% 
    group_by(канал.продаж, сезон.продаж) %>% 
    #select(канал.продаж, пол, кол.во.sku) %>%
    summarise(n = n()) %>% 
    mutate(freq = n / sum(n))


df$percent <- df$percent*100

df <- ddply(df, .(X),
                     transform, pos = cumsum(percent) - (0.5 * percent))

fill_5 <- c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")
fill_4 <- c("#f7f7f7", "#cccccc", "#969696", "#525252")
fill_3 <- c("#f0f0f0", "#bdbdbd", "#636363")
fill_2 <- c("#cccccc", "#969696")




ggplot() + geom_bar(aes(y = percent, x = X, fill = Variable), data = df,  stat="identity") + 
    geom_text(data=df, aes(x = X, y = pos, label = paste0(SKU, " SKU ", "\n", round(percent,0),"%")),
              size=3) + xlab("") + ylab("") +
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
    theme(legend.position="bottom", legend.direction="horizontal",
          legend.title = element_blank()) +
    ggtitle("Free style mix by Type") +
    scale_fill_manual(values=fill_5)


