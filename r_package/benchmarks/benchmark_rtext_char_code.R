library(diffrprojects)
library(microbenchmark)
library(pryr)

source("benchmarks/rtext_char_code1.R")
source("benchmarks/rtext_char_code2.R")

init1 <- function(){
  tmp1 <- rtext_char_chode1$new(text_file=dp_tf("rc_3.txt"))
  for(i in 1:100){
    tmp1$char_code("hodor", sample(1:10, 1000, replace=TRUE), sample(tmp1$info()$character, 1000) )
  }
  cat(".")
}

init2 <- function(){
  tmp2 <- rtext_char_chode2$new(text_file=dp_tf("rc_3.txt"))
  for(i in 1:100){
    tmp2$char_code("hodor", sample(1:10, 1000, replace=TRUE), sample(tmp2$info()$character, 1000) )
  }
  cat(".")
}

microbenchmark(
  init1(),
  init2(),
  times=10
)
