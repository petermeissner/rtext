library(diffrprojects)
library(magrittr)
library(dplyr)

dings <- rtext$new(text_file=dp_tf(1))
#dings$char_data_set("pimpf", c(1:10,20:22,30:35), rep(1, length(c(1:10,20:22,30:35))))
#dings$char_data_set("pompf", 5:11, 1)
dings$char_data_set("pimpf", 1:30, 2)
dings$char_data_set("pompf", 1:30, 1:30)

char_data <- dings$char_data_get()
token     <- dings$token_get() %>% head

char_data_names <- names(char_data)[-(1:2)]
char_data <- data.frame(char_data$i, char_data$i, char_data$i, char_data$char, char_data[, -c(1,2)])
names(char_data) <- c("char_i", "from", "to", "char", char_data_names)

token   <- data.frame(token_i=seq_dim1(token), token)
token_i <- which_token(char_data$char_i, token$from, token$to)
char_data$token_i <- token_i


token_data_get = function(FUN="modus", multimodal = NA, warn = FALSE, ...){
  if(FUN=="modus"){
    char_data[,-c(1:4)] %>%
      aggregate(by=list(token_i), FUN="modus", multimodal=multimodal, warn=warn)
  }else{
    char_data[,-c(1:4)] %>%
      aggregate(by=list(token_i), FUN=FUN, ...)
  }
}

dings$token_data_get("modus", FALSE)
dings$token_data_get("mean")


private <- list()
private$char_data <- dings$get("char_data")

# get_token()

# entering data
# char data to token data
# token data to char data

# token_aliases?

# plot text - polygons?

# extensive testing on init!!!

