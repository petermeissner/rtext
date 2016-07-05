library(diffrprojects)
library(magrittr)
library(dplyr)

#dings <- rtext$new(text_file=dp_tf(1))
dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
dings$char_data_set("pimpf", 1:30, 2)
dings$char_data_set("pompf", 1:30, 1:30)

dings$token_data_get(FUN="modus")
dings$token_data_get(FUN="mean")
dings$token_data_get(FUN="modus", multimodal=NA, warn = FALSE)

dings$token_data_get()



# get_token()

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?

# extensive testing on init!!!

