#' function providing basic information on texts within diffrproject
#' @param dp a diffrproject object
#' @export
dp_text_base_data <- function(dp){
  df <- data.frame()
  rt <- rtext$new("", verbose=FALSE)$info()
  names <- names(rt)
  for(i in seq_along(names) ){
    df[seq_along(dp$texts), names[i]] <- NA
  }
  for( i in seq_along(dp$texts) ){
    df[i,] <- get("info", dp$texts[[i]])()
  }
  df
}
