superheroes <- "
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, skip = 1)

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, skip = 1)


Joined_heroes <- superheroes %>% left_join(publishers, by = "publisher")
Joined_heroes


#library to work with date
library(anytime)
db$icoStart <- anydate(as.ordered(db$icoStart))


#filter for country
df1 <- df %>% filter (icoCountry == "Panama")


#success rate
nrow (  df1 %>% filter(icoRaisedTotal > 0 ) ) / nrow (  df1 %>% filter(icoRaisedTotal != 0 ) )

#avg rating
mean(df1$icoRating)

#avg raised
sum(df1$icoRaisedTotal)/1e6  /  nrow (  df1 %>% filter(icoRaisedTotal > 0 ) )



#check if there are any dates before 2018-02-26 (YYYY-MM-DD)
db %>% filter (icoStart < '2018-02-26')


db$icoRating <- as.numeric(levels(db$icoRating))[db$icoRating]
mean(df1)

df1 <- as.double(db$icoRating)


#   copy data from clipboard
library(clipr)
my_data <- read_clip_tbl(read_clip(), header = T,  sep = "\t")

# Options to pass to read.table. The following read.table arguments will be passed by default, but can be overridden by specifying them when calling read_clip_tbl:
# 
# header
# TRUE
# sep
# "\t"
# stringsAsFactors
# FALSE
# na.strings
# c("NA", "")
# strip.white
# TRUE

summary(db$icoHardCapFiat)/1e6
round(mean(db$icoHardCapFiat, na.rm = T)/1e6, 2)

which.max(db$icoHardCapFiat)
db[171,]

tail(db,5)


db$icoHardCapFiat[db$icoId == 3038]/1e9

db[which(db$icoId == 3038),]


