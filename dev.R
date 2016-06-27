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


x  <- sample(char_data$char_i)
y1 <- token$from
y2 <- token$to

which_token <- function(x, y1, y2){
  # how to order x and y?
  order_x <- order(x)
  order_y <- order(y1)
  # order x and y! - which_token_worker expects inputs to be ordered
  ordered_x  <- x[order_x]
  ordered_y1 <- y1[order_y]
  ordered_y2 <- y2[order_y]
  # doing-duty-to-do
  index <- which_token_worker(ordered_x, ordered_y1, ordered_y2)
  # ordering back to input ordering
  index <- order_y[index[order(order_x)]]
  # return
  index
}



which_token(char_data$char_i, token$from, token$to)



# get_token()

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?

# extensive testing on init!!!

