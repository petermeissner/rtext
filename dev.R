library(rtext)

# original
dings_original <-
  rtext$new(
    text_file = rtext:::testfile("test_utf8.txt"),
    id        = "bollocks"
  )
# saving
test_db <- tempfile()
dings_original$export_sqlite(test_db)



library(RSQLite)

con <- dbConnect(SQLite(), test_db)
dbGetInfo(con)
dbListTables(con)


##################

dings  <- rtext$new(text="aakdfhg.kadfsgkl.dasdfgasdfgasdfgsagsdfgsdgsd")
dings$char_data_set_regex("a", "a", 1)
dings$char_data_get()
dings$debug()
