library(diffrprojects)
library(magrittr)
library(dplyr)
library(hellno)



text_path  <- "C:/Users/peter/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)


dp <- diffrproject$new()

for( i in seq_along(text_files)){
  tmp <-
    rtext$new(
      text_file   = text_files[i],
      encoding    = "latin1",
      tokenize_by = "\n"
    )
  dp$text_add(tmp, name=basename(text_files[i]))
}


dp$text_data()



names(dp$texts)


devtools::install_github("petermeissner/diffr")

diffr::diffr(
  text1 = dp$texts[[1]]$text_get(),
  text2 = dp$texts[[2]]$text_get()
)
