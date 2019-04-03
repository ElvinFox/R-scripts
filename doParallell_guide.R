# Demo of returning multiple results from a %dopar% loop.
library(foreach)
library(doParallel)
library(ggplot2)

cl <- makeCluster(3)
registerDoParallel(cl)

# Create class which holds multiple results for each loop iteration.
# Each loop iteration populates two properties: $resultPlot and $resultMessage.
# For a great tutorial on S3 classes, see: 
# http://www.cyclismo.org/tutorial/R/s3Classes.html#creating-an-s3-class
plotAndMessage <- function(resultPlot=NULL,resultMessage="?")
{
  me <- list(
    resultPlot = resultPlot,
    resultMessage = resultMessage
  )

  # Set the name for the class
  class(me) <- append(class(me),"plotAndMessage")
  return(me)
}

oper <- foreach(i=1:5, .packages=c("ggplot2")) %dopar% {

  x <- c(i:(i+2))
  y <- c(i:(i+2))
  df <- data.frame(x,y)
  p <- ggplot(df, aes(x,y))
  p <- p + geom_point()

  message <- paste("Hello, world! i=",i,"\n",sep="")

  result <- plotAndMessage()
  result$resultPlot <- p
  result$resultMessage <- message
  return(result)
}

# Print resultant plots and messages. Despite running on multiple cores,
# 'foreach' guarantees that the plots arrive back in the original order.
foreach(i=1:5) %do% {
  # Print message attached to plot.
  cat(oper[[i]]$resultMessage)
  # Print plot.
  print(oper[[i]]$resultPlot)
}

stopCluster(cl)
