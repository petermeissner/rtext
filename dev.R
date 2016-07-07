library(diffrprojects)
library(magrittr)
library(dplyr)


dp <- diffrproject$new()
dp



self <- rtext$new(text_file=dp_tf(1))
self$char_data_set("var1", sample(1:10000, 300), 1)


lines <- c(1,1,1)

res   <- character(length(lines))
lines <- self$text_lines()[lines,]
from  <- lines$from
to    <- lines$to
for( i in seq_along(from) ){
  res[i] <- self$text_get(from=from[i], to=to[i])
}




tmp <- self$text_lines()
self$text_lines_get(1)
self$text_lines_get(4)
self$text_lines_get(1:4, TRUE)
self$text_lines_get(1:4, FALSE)





x         <- nchar(dings$text_get(split = "\n"))+1
char_data <- dings$char_data_get()
lines     <- data.frame(from=)
y <- seq_along(x)
plot(
  x,y,
  pch=".",
  type = "l",
  col="grey",
  ylab="line",
  xlab="char",
  asp = 10,
  xlim = c(0, (ceiling(max(x)/10^nchar(max(x))))*10^nchar(max(x)) )
)




# create rtext$lines method
# add line information to char_data / to char??



# plot text - polygons?



