df <- as.data.frame(HairEyeColor)

df1 <- as.data.frame(HairEyeColor["Brown",,"Female"])
chisq.test(df1)


library(ggplot2)
df <- diamonds
head(df)


df <- diamonds
main_stat <- chisq.test(df$cut, df$color)[[1]]



df <- diamonds

df$factor_price <- as.factor(  df$price > mean(df$price) )
levels(df$factor_price) <- rep( c(0,1) )

df$factor_carat <- as.factor(  df$carat > mean(df$carat) )
levels(df$factor_carat) <- rep( c(0,1) )

main_stat <- chisq.test(df$factor_price, df$factor_carat)[[1]]



fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value

plot(mtcars$am, mtcars$vs)

df <- iris
ggplot(df, aes(x = Sepal.Length)) +
        geom_histogram(fill =  "white", col = "black", binwidth = 0.4) +
        facet_grid(Species~.)

ggplot(df, aes(Sepal.Length, fill = Species)) +
        geom_density(alpha = 0.6)

ggplot(df, aes(Species,Sepal.Length )) +
        geom_boxplot(show.legend = T, outlier.size = 5, outlier.alpha = 0.7, col = "blue")


df <- ToothGrowth

one <- subset(df, supp == "OJ" & dose == 0.5)
two <- subset(df, supp == "VC" & dose == 2)

t_stat <- t.test(one$len, two$len )$statistic

df <- read.csv("https://stepic.org/media/attachments/lesson/11504/lekarstva.csv")
head(df)

t.test(df$Pressure_before, df$Pressure_after, paired = T)






df <- read.table("C:\\Users\\ElvinFox\\Downloads\\dataset_11504_15 (6).txt")

bartlett.test(df$V1 ~ df$V2)

t.test(df$V1 ~ df$V2, var.equal = TRUE)
wilcox.test(df$V1 ~ df$V2)



df <- read.table("C:\\Users\\ElvinFox\\Downloads\\dataset_11504_16 (1).txt")

pv <- t.test(df$V1,  df$V2)$p.value

mean(df$V1)
mean(df$V2)
pv


df <- npk
fit <- aov(yield ~ N + P + K, data = df)
summary(fit)


df <- iris
fit <- aov(df$Sepal.Width ~ df$Species)
summary(fit)

TukeyHSD(fit)




df <- read.csv("https://stepik.org/media/attachments/lesson/11505/Pillulkin.csv")
df$patient <- as.factor(df$patient)

fit <- aov(temperature ~ pill + Error(patient/pill), data = df)

summary( fit)


fit1 <- aov(temperature ~ doctor * pill  + Error( patient/(pill * doctor) ), data = df)        
summary( fit1)


library(Hmisc)
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp)) +
        stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2)) +
        stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2)) +
        stat_summary(fun.data = mean_cl_boot, geom = 'line',  position = position_dodge(0.2))
obj


my_vector <- c(1, 2, 3, NA, NA, NA, NA)
NA.position(my_vector)
# 4 5

NA.position <- function(x){
        which(is.na(x))
}
NA.position(my_vector)


NA.counter <- function(x){
        summary(my_vector)[[7]]
}

NA.counter(my_vector)

summary(my_vector)[[7]]



filtered.sum(c(1, -2, 3, NA, NA))
# [1] 4 
x <- c(-1, -2, -3, NA, NA)


filtered.sum <- function(x){
        z <- which(x > 0)  
        sum(x[z])
}


x <- c(0.13, 0.03, -0.11, 39.76, 0.14, 10.02, -0.25,
       -1.21, -0.34, 2.12, 0.82, 1.78, -1.06, 1.3, -0.19, 0.88,
       -4.49, -0.21, 0.43, 0.69, 8.47, 0.09, 0.83, 0.16, -2.52, -0.15, -0.9, -2.17, -17.68, -1.05)
boxplot(x)
length(x)

outliers.rm <- function(x){
        
        qa <- quantile(x, probs = c(0.25, 0.75)) #рассчитывает первый и третий квартиль вектора x 
        qr <- IQR(x)   #рассчитывает межквартильный размах вектора x
        rm <- which(x < (qa[1] - 1.5 * qr) | x > (qa[2] + 1.5 * qr))
        x <- x[-rm]
        return(x)
}

outliers.rm(x)



x <- mtcars[, c(1,5)]

corr.calc <- function(x){
        vals <- cor.test(x[,1], x[,2]) 
        return(c(vals$estimate, vals$p.value))
}

corr.calc( iris[,1:2] )




x <-  read.csv("https://stepik.org/media/attachments/lesson/11504/step6.csv")


filtered.cor <- function(x){
        nm <- colnames(x)
        c <- as.vector( c() )
        for (i in (1:length(nm)) ) {
                if ( is.numeric(x[,i])  == T) c <- rbind(c,i)
        }
        cleared <-  x[, c] 
        stat <- cor(cleared)
        stat[lower.tri(stat,diag = T)] <- 0
        stat <- as.vector( stat )
        stat[ which.max(  abs(stat) ) ]
        
}

filtered.cor(iris)
filtered.cor(x)

filtered.cor(test_data)

iris$Petal.Length <- -iris$Petal.Length
filtered.cor(iris)


for (i in (1:length(nm)) ) {
        if ( is.null( levels(x[,nm[i]] )) == T) c <- rbind(c,i)
}
is.numeric( sum(x[,1]) )
is.numeric(x[,2])


test_data <- as.data.frame(list(V5 = c("j", "j", "j", "j", "j", "j", "j", "j"), 
                                V1 = c(1.3, -0.8, -0.7, 0.6, -0.6, 0.4, 0.6, -0.3), 
                                V4 = c("k", "k", "k", "k", "k", "k", "k", "k"), 
                                V6 = c("e", "e", "e", "e", "e", "e", "e", "e"), 
                                V3 = c(-0.3, -2.3, 0.7, -1, 0.8, -0.4, -0.1, -1.3),
                                V2 = c(-0.3, -2.3, 0.7, -1, 0.8, -0.4, -0.1, -1.3)))

mtx <- cor(test_data[,c(2,5:6)])

mtx[lower.tri(mtx,diag = T)] <- 0
mtx


test <- sapply(x, function(x) is.numeric(x))


test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)
#[1] -0.1031003

smart_cor <- function(x){
       t1 <- shapiro.test(x[,1])$p.value
        t2 <- shapiro.test(x[,2])$p.value
        result <- ifelse( (t1 < 0.05 |t2 < 0.05), 
                          cor.test(x[[1]], x[[2]], method = "spearman")$estimate, 
                          cor.test(x[[1]], x[[2]], method = "pearson")$estimate )
        return(result)
}

test_data[,2]

x  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")

z <- spearman.test(x)
z$estimate
t1$p.value

test_data <- as.data.frame(list(col1 = c(-0.78, 1.74, 2, -0.3, 0.44, 0.39, -0.82, 0.88, 1.48, -1.79, 0.08, 1.24, -0.47, 1.68, -1.16, -1.79, -0.51, -1.72, -0.4, 0.43, -0.66, 0.88, 1.93, 1.3, 1.79, -0.78, 1.59, 0.04, -1.27, -1.25), 
                                col2 = c(-0.19, -0.14, -0.35, -0.49, 1.6, -0.19, 0.15, 0.77, 0.02, -0.54, -0.85, 1.18, -0.23, 0.58, -1.26, 0.36, -0.78, -1.04, -0.49, -3.62, -0.63, 0.44, 1.17, 0.56, 0.45, 0.65, 0.16, 1.03, 0.46, 1.41)))


library(dplyr)
library(ggplot2)

df <- subset(diamonds, cut == "Ideal" & carat == 0.46 )
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients

levels(df$cut)


x <- iris[,1:2]
my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
regr.calc(iris[,1:2]) # переменные значимо не коррелируют 

#[1] "There is no sense in prediction"



regr.calc <- function(x){
        ifelse(cor.test(df[[1]], df[[2]], method = "pearson")$p.value < 0.05,
        df$fit <<- lm(df[[1]] ~ df[[2]])$fitted.values,
        "There is no sense in prediction")
}

x <- iris[,c(1,4)]
my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
regr.calc(my_df) # переменные значимо коррелируют 
rm(x)



length(test_data[,1])


test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(df){
        model <- lm(y ~ x_1 + x_2, df)
        df$y_full = ifelse( is.na(df$y) , predict(model, df), df$y )
        return(df)
}
fill_na(test_data)



x <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")




x1 <- x[complete.cases(x),]
model <- lm(y ~ x_1 + x_2, data = x1)

summary(model)

new.df <- data.frame(x_1 = 10, x_2 = 45 )
predict(model, new.df)

predict(model, x[10,])

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
mtcars$wt_centered <- mtcars$wt - mean(mtcars$wt)

df <- lm(mtcars$mpg ~ mtcars$wt_centered * mtcars$am)
summary(df)



library(ggplot2)
# сначала переведем переменную am в фактор
mtcars$am <- factor(mtcars$am)

# теперь строим график
my_plot <- ggplot(data = mtcars, aes(x = wt, y = mpg, col = am) ) + 

        geom_smooth(method = 'lm')
my_plot


df <- mtcars[c('wt', 'mpg', 'disp', 'drat', 'hp')]

test <- lm(df$wt ~ df$mpg + df$disp + df$drat + df$hp)
test <- lm(df$wt ~ df$mpg + df$disp  + df$hp)
summary(test)

df <- attitude
summary(lm(df$rating ~ df$complaints * df$critical))




my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 
               0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 
               0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 
               0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 
               0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 
               0.024, 0.049, 0.431, 0.061, 0.523)

hist(my_vector)

shapiro.test(log(my_vector))
shapiro.test(1/my_vector)



beta.coef <- function(x){
        df <- as.data.frame( scale( x ) )
        fit <- lm(df[,1] ~ df[,2], df)  
        return(fit$coefficients)
}

beta.coef(mtcars[,c(1,3)])
df <- as.data.frame( scale( mtcars[,c(1,3)] ) )
z <- lm(df[,1] ~ df[,2], df)

beta.coef(mtcars[,c(1,3)])
beta.coef(swiss[,c(1,4)])




model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full), direction = "forward")


summary(ideal_model)


anova(ideal_model, model_full)


df <- LifeCycleSavings


##all interactions including second level
summary( lm(sr ~ (.)^2, df) )
