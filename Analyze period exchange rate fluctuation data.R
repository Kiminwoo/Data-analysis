library(xlsx)
library(nnet)

data3 <- read.xlsx2(file.choose(),1)

data3

data3$현찰파실때 <- gsub(",","",data3$현찰파실때)

data3$현찰파실때

data3$현찰파실때 <- as.numeric(data3$현찰파실때)

df <- data.frame(일자=data3$일자,현찰파실때=data3$현찰파실때)

df <- df[order(df$일자)]

n <- nrow(df)

rownames(df) <- 1:n

Pnorm <-(df$현찰파실때-min(df$현찰파실때)) / (max(df$현찰파실때)-min(df$현찰파실때)) * 0.9 + 0.05

df <- cbind(df,현찰파실때norm=Pnorm)

df

n80 <- round(n*0.8,0)

df.learning <- df[1:n80,]
df.learning

df.test <- df[(n80+1):n,]
df.test

INPUT_NODES <- 10
HIDDEN_NODES <- 10
OUTPUT_NODES <- 5
ITERATION <- 100

getDataSet <- function(item,from,to,size)
{
  dataframe <- NULL
  to <- to-size +1
  for(i in from:to)
  {
    start <- i
    end <- start + size -1
    temp <- item[c(start:end)]
    dataframe <- rbind(dataframe,t(temp))
  }
  return(dataframe)
}

in_learning <- getDataSet(df.learning$현찰파실때norm,1,92,INPUT_NODES)

in_learning

out_learning <- getDataSet(df.learning$현찰파실때norm,11,97,OUTPUT_NODES)

out_learning


model <- nnet(in_learning, out_learning, size = HIDDEN_NODES, maxit = ITERATION)

model

in_test <- getDataSet(df.test$현찰파실때norm,1,19,INPUT_NODES)
in_test

predicted_values <- predict(model, in_test, type="raw")

predicted_values

Vpredicted <- (predicted_values-0.05)/0.9*(max(df$현찰파실때)-min(df$현찰파실때)) + min(df$현찰파실때)

Vpredicted

Vreal <- getDataSet(df.test$현찰파실때,11,24,OUTPUT_NODES)

Vreal

ERROR <- abs(Vreal - Vpredicted)
MAPE <-rowMeans(ERROR / Vreal) * 100

MAPE

mean(MAPE)


in_forecasting <- df$현찰파실때norm[30:40]

in_forecasting

predicted_values <- predict(model,in_forecasting,type="raw")

Vpredicted <-(predicted_values -0.05) / 0.9*(max(df$현찰파실때)-min(df$현찰파실때)) + min(df$현찰파실때)

Vpredicted

plot(30:80 , df$현찰파실때[30:80], xlab="일자" , ylab="현찰파실때", xlim=c(31,85), ylim=c(1000,2000), type="o")

lines(81:85 , Vpredicted , type="o" , col="red")
abline(v=80, col="blue", lty=2)