#### ---------------------------------------------------------------------------

library(diffrprojects)
library(magrittr)
library(dplyr)
library(hellno)

#### ---------------------------------------------------------------------------


text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)

dings = rtext$new(text_file=text_files[1], encoding="latin1")


dings$get("text")()
tmp <- text_locate_all(dings$get("text")(), "Lesung")
unique(as.integer(mapply(seq, tmp[[1]]$start, tmp[[1]]$end)))

TEXT = "Outside of a dog, a book is man's best friend. Inside of a dog it's too dark to read."

char_data_set_regex = function(x=NULL, regex=NULL, val=NA, ...){
  found_spans <- text_locate_all(TEXT, regex, ...)[[1]]
  found_is    <- unique(as.integer(unlist(mapply(seq, found_spans$start, found_spans$end))))
  list(x, found_is, val)
}


char_data_set_regex("blob", "dog", NA)
char_data_set_regex(x="dog_friend", regex="dog|friend", val=TRUE)
