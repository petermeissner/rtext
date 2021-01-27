library(diffrprojects)
library(microbenchmark)
library(pryr)

source("benchmarks/which_token_functions.R")

x  <- sort(sample(1:100000, 10000))

y <- sort(sample(2:99998,10000))
y1 <- c(1,y+1)
y2 <- c(y,100000)

microbenchmark(
  fw1(x,y1,y2),
  fw2(x,y1,y2),
  f_fw2(x,y1,y2),
  f_fw3(x,y1,y2),
  f_fw4(x,y1,y2),
  f_fw5(x,y1,y2),
  f_fw6(x,y1,y2),
 times=10
)

length(y)*length(x)
