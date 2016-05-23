library(diffrprojects)

#dongs <- rtext$new(file=dp_tf(1))
dings <- rtext$new(text="1234567890")

dh_1 <- dings$data_hash()
dh_2 <- dings$data_hash()

dings$char_add("a")
dh_3 <- dings$data_hash()

dings$char_delete(1)
dh_4 <- dings$data_hash()

dings$char_add("a")
dh_2 <- dings$data_hash()

dings$char_add("a")
dh_2 <- dings$data_hash()

dings$char_code()



# get_token()

# deleting text
# insert text
# replace text

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?
