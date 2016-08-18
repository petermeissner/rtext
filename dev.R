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
