# lines of code
library(diffrprojects)
source_files <- list.files(path=c("R","src"), pattern = "(.R$)|(.cpp$)", recursive = TRUE, full.names = TRUE)
dings <- lapply(source_files, text_read)

df <-
  data.frame(
    file=basename(source_files),
    char=nchar(dings),
    lines=unlist(lapply(unlist(lapply(dings, strsplit, "\n"), recursive = FALSE), length))
  )

sum(df$lines)
sum(df$char)

