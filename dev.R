library(diffrprojects)

system.time(dings <- rtext$new(file=dp_tf(1)))


dings <- rtext$new(text="1234567890")
dings$add("1231")
dings$get_text()
dings$add("//////")
dings$get_text()

dings$get_text()
dings$get_text(from = 1, to=5)
dings$get_text(from=200)
dings$get_text(to=200)





# get_token()

# deleting text
# insert text
# replace text

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?

devtools::load_all()
chars <- unlist(strsplit("123456789",""))
from=NULL; to=NULL; split=NULL
length <- 100

rtext_get_text(chars)
