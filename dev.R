library(diffrprojects)
library(magrittr)
library(dplyr)


dp <- diffrproject$new()
dp



dings <- rtext$new(text_file=dp_tf(1))
dings <- rtext$new(text=paste0(sample(c(letters, LETTERS, "\n"), 3000, TRUE), collapse = ""))
dings <- rtext$new(text=c("111","444444444444444","1", "1", "1"))


dings$char_data_set("var1", c(1:3), 1)
plot(dings, "var1")
plot(dings, "var1", lines=1:2)



# create rtext$lines method
# add line information to char_data / to char??



# plot text - polygons?



