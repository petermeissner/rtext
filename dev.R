library(diffrprojects)

system.time(dings <- rtext$new(file=dp_tf(1)))


dings <- rtext$new(text="1234567890")

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

