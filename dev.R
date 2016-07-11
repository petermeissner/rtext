library(diffrprojects)
library(magrittr)
library(dplyr)



text_path  <- "C:/Users/peter/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)


dp <- diffrproject$new()

for( i in seq_along(text_files)){
  dp$text_add(
    rtext$new( text_file=text_files[i], encoding="latin1" ) ,
    name = i
  )
}


dp$texts[[1]]$text_show()
dp$texts[[2]]$text_show()





names(dp$texts)
