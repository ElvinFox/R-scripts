


################### < 2 > ####################################################################################################################
smart_test <-  function(x) {
    T <- table(x)
    if( all(T >= 5) == TRUE) {
        CT <- chisq.test(T)
        print(c(CT$statistic[[1]], CT$parameter[[1]], CT$p.value))
    }
    else {
        FT <- fisher.test(T) 
        print(FT$p.value)
    }
}

#### master's solution
    importance_calc <- function(v1, v2, threshold=3){    
    ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
    iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))
####

cat("\f")  

################### < 3 > ####################################################################################################################
most_significant <-  function(x){
    names <- colnames(x)
    cl <- c(1:as.numeric(length(x)))
    result <- c()
    for(val in cl){
        pv <- chisq.test(table(x[val]))$p.value
        result <- append(result, pv)
    }
    return(names[which(result == min(result))])
}

#### master's solution
    most_significant  <- function(test_data){    
    chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
    min_p  <- which(chisq_tests == min(chisq_tests))    
    return(colnames(test_data)[min_p])
    }
####


################### < 4 > ####################################################################################################################
iris$important_cases <- factor(c("Yes","No"))
M <- c(mean(iris$Sepal.Length), mean(iris$Sepal.Width), mean(iris$Petal.Length), mean(iris$Petal.Width))

for(r in c(1:as.numeric(length(iris[,1]))) ){
    count = 0
    for (c in c(1:4)){
        if (iris[r,c]>= M[c]) {
            count <-  count+1
        }
        if(count>=3){iris[r,6] <- 'Yes'}
        else {iris[r,6] <- 'No'}
    }
}

    #### master's solution
        importance_calc <- function(v1, v2, threshold=3){    
        ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
        iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))
    ####

################### < 5 > ####################################################################################################################
test_data <- as.data.frame(list(V1 = c(19, 32, 15, 22, 23, 22, 14, 15), 
                                V2 = c(17, 14, 18, 20, 17, 17, 20, 20), 
                                V3 = c(20, 19, 22, 17, 18, 20, 26, 15), 
                                V4 = c(15, 20, 18, 16, 23, 16, 19, 19)))


get_important_cases <- function(test_data){
    q <- apply( test_data, 2, function(test_data) test_data>mean(test_data) )
    test_data$important_cases <- factor( ifelse( apply(q,1,sum) >length(test_data)/2 ,"Yes", "No"),levels = c("Yes", "No"))
    return(test_data)
}
get_important_cases(test_data)


################### < 6 > ####################################################################################################################
x <- c(3,20,19,19,4,8,5,5,11,6,11,5,7,9,17)


stat_mode <- function(x){
    Q <- as.numeric(which(table(x) == max(table(x) ) ))
    moda <- as.data.frame(table(x)[Q] )
    return( as.vector(moda[,1])    ) 
}

stat_mode(x)

moda <- as.data.frame( table(x)[as.numeric(which(table(x) == max(table(x) ) ))]  )

T <- table(x)
str(T)
summary(T)
T[1]
    

u <- unique(x)

for (i in u){}

