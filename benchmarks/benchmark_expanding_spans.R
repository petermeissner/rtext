library(diffrprojects)
library(magrittr)
library(microbenchmark)

dings <- rtext$new(text_file=dp_tf("rc_1.txt"), verbose=TRUE)
dings$get("char_data")

dings$char_data_set("pimpf", 1, 1)
dings$get("char_data")

dings$char_data_set("pompf", 10, 1)
dings$get("char_data")

dings$char_data_set("pampf", c(1,10), 1)
dings$get("char_data")

dings$char_data_set("pampf", c(2), 1)
dings$get("char_data")


df1 <- data.frame()
df2 <- data.frame(i=1, pimpf=2, char="D")

rbind_fill(df2,df1)




