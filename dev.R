library(diffrprojects)
library(magrittr)
library(dplyr)
library(hellno)



text_path  <- "C:/Users/peter/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)


dp <- diffrproject$new()
dp$text_add(
    rtext = rtext$new(
      text_file   = text_files[1],
      encoding    = "latin1",
      tokenize_by = "\n"
    ),
    name = basename(text_files[1])
  )

dp$text_add(
  rtext = rtext$new(
    text_file   = text_files[2],
    encoding    = "latin1",
    tokenize_by = "\n"
  ),
  name = basename(text_files[2])
)

