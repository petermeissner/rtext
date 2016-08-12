

#' storage for internals
dp_storage <- new.env(parent = emptyenv())


# #' list of ready to use functions for rtext initialization and tokenization
# #' @export
# rtext_tokenizer_list <- list(
#   words  = function(x){text_tokenize_words(x, non_token = TRUE )},
#   words2 = function(x){text_tokenize_words(x, non_token = FALSE)},
#   lines  = function(x){text_tokenize(x, "\n", non_token = TRUE)}
# )


#' function to get text from rtext object
#'
#' @param chars the chars field
#' @param length number of characters to be returned
#' @param from first character to be returned
#' @param to last character to be returned
#' @keywords internal
# #' @export
rtext_get_character <- function(chars, length=100, from=NULL, to=NULL){
  # helper functions
  bind_to_charrange <- function(x){bind_between(x, 1, length(chars))}
  bind_length       <- function(x){bind_between(x, 0, length(chars))}
  return_from_to    <- function(from, to, split){
    res  <- chars[seq(from=from, to=to)]
    return(res)
  }
  # only length
  if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
    length <- max(0, min(length, length(chars)))
    length <- bind_length(length)
    if(length==0){
      return("")
    }
    from   <- 1
    to     <- length
    return(return_from_to(from, to, split))
  }
  # from and to (--> ignores length argument)
  if( !is.null(from) & !is.null(to) ){
    from <- bind_to_charrange(from)
    to   <- bind_to_charrange(to)
    return(return_from_to(from, to, split))
  }
  # length + from
  if( !is.null(length) & !is.null(from) ){
    if( length<=0 | from + length <=0 ){
      return("")
    }
    to   <- from + length-1
    if((to < 1 & from < 1) | (to > length(chars) & from > length(chars) )){
      return("")
    }
    to   <- bind_to_charrange(to)
    from <- bind_to_charrange(from)
    return(return_from_to(from, to, split))
  }
  # length + to
  if( !is.null(length) & !is.null(to) ){
    if( length<=0 | to - (length-1) > length(chars) ){
      return("")
    }
    from <- to - length + 1
    if((to < 1 & from < 1) | (to > length(chars) & from > length(chars) )){
      return("")
    }
    from <- bind_to_charrange(from)
    to   <- bind_to_charrange(to)
    return(return_from_to(from, to, split))
  }
  stop("rtext$get_character() : I do not know how to make sense of given length, from, to argument values passed")
}



#' function for plotting rtext
#' @export
#' @param x object of class rtext
#' @param y not used
#' @param what char_data to be plotted
#' @param lines vector of integer listing the lines to be plottted
#' @param col color of the char_data variable to be highlighted
#' @param ... further parameters passed through to initial plot
plot.rtext <-
  function(
    x,
    y         = NULL,
    lines     = TRUE,
    what      = NULL,
    col       = "#ED4C4CA0",
    ...
  ){
    # preparing data
    line_data  <- subset(x$text_get_lines(), lines)
    plot_x     <- line_data$n
    plot_y     <- line_data$line
    max_plot_y <- max( plot_y )
    plot_y     <- abs( plot_y - max_plot_y ) + 1
    max_plot_x <- max( plot_x )


    # plotting text lines
    graphics::plot(
      x    = plot_x,
      y    = plot_y,
      type = "n",
      ylab = "line",
      xlab = "char",
      xlim      = c(0, (ceiling(max_plot_x)/10^nchar(max_plot_x)*10)*(10^nchar(max_plot_x)/10) ),
      ylim      = c(0, max_plot_y + 1 ),
      ...,
      axes=FALSE
    )
    graphics::axis( 1 )
    graphics::axis( 2, c(max_plot_y, 1), c(1, max_plot_y) )
    graphics::box()
    graphics::rect(
      xleft   = 0,
      xright  = plot_x,
      ybottom = plot_y - 0.5,
      ytop    = plot_y + 0.5,
      col = "grey", border = "grey", lty=0
    )
    # plotting char_data
    if ( !is.null(what) ){
      char_data <-
        x$char_data_get(
          x    = what,
          from = min(line_data$from),
          to   = max(line_data$to)
        )

      index <- which_token( char_data$i, line_data$from, line_data$to)
      plot_what_x <- char_data$i - line_data[ index, ]$from
      plot_what_y <- line_data[ index, ]$line
      plot_what_y <- abs( plot_what_y - max_plot_y ) +1
      graphics::rect(
        xleft   = plot_what_x,
        xright  = plot_what_x + 1,
        ybottom = plot_what_y - 0.5,
        ytop    = plot_what_y + 0.5,
        col = col, border = col, lty=0
      )
    }
    # return
    if(!exists("char_data")){char_data<-NULL}
    return(
      invisible(
        list(
          line_data = line_data,
          char_data = char_data
          )
        )
      )
  }
















