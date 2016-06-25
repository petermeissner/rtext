library(diffrprojects)
library(magrittr)
library(dplyr)

dings <- rtext$new(text_file=dp_tf(1))
dings$char_data_set("pimpf", c(1:10,20:22,30:35), rep(1, length(c(1:10,20:22,30:35))))
dings$char_data_set("pompf", 5:11, 5:11)


char_data <- dings$char_data_get()
token     <- dings$token_get() %>% head

char_data_names <- names(char_data)[-(1:2)]
char_data <- data.frame(char_data$i, char_data$i, char_data$i, char_data$char, char_data[, -c(1,2)])
names(char_data) <- c("char_i", "from", "to", "char", char_data_names)

token <- data.frame(token_i=seq_dim1(token), token)


merger <- merge_spans(char_data, token)

merger %>% left_join(char_data)


dummy <- function(x, y1, y2) which(y1 <= x & x <= y2)

which_token(char_data$char_i, token$from, token$to)



dings$text_show()


# get_token()

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?

# extensive testing on init!!!

