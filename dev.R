library(rtext)
dings     <- rtext_base$new("meine mudder schneidet speck du spack")
private   <- dings$get("private")
self   <- dings$get("self")
char_data <- dings$get("char_data")

##

dings$char_data_set("x1", 1:3, 1)
dings$char_data_set("x1", 1)
dings$char_data_set("x2", 1:5, 2)
dings$char_data_set("x3", 10:5, 3)
dings$char_data_set("x4", 10:5, 4)
dings$char_data_set("x5", 17, 5)


dings$char_data_get()
res <- dings$char_data_get(x="x1", full=TRUE)

sm <- sum(names(res) %in% c("i","char"))
ln <- sum(!(names(res) %in% c("i","char")))
seq_len(ln)+sm
res[seq_len(ln)+sm]




dings <- rtext$new("123")
private   <- dings$get("private")
self   <- dings$get("self")
char_data <- dings$get("char_data")
