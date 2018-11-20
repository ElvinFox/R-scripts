df <- read.csv2("~/Downloads/SC&HC to fix.csv")


df$icoHardCap <- as.character(df$icoHardCap)
df$icoSoftCap <- as.character(df$icoSoftCap)


df$icoSoftCap_1 <- gsub("[^0-9\\.]", "", df$icoSoftCap) 
df$icoSoftCap_2 <- gsub("[^A-Z]", "", df$icoSoftCap) 

df$icoHardCap_1 <- gsub("[^0-9\\.]", "", df$icoHardCap) 
df$icoHardCap_2 <- gsub("[^A-Z]", "", df$icoHardCap) 

write.csv(df, file = "MyData.csv")
