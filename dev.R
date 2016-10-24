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


dings$tokenize_data_lines()



##################

dings  <- rtext$new(text=prometheus_early)
dings$char_data_set_regex("du", "du|Du", 1)
dings$char_data_set_regex("ich", "ich|Ich", 1)
dings$char_data_get()

dings$tokenize_data_lines()

dings$tokenize_data_sequences( data.frame(from=c(1,50),to=c(100,250)) )

dings$debug()

dings$tokenize_data_regex("\n")
