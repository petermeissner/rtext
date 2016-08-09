#' some helper function
my_function <- function(){
  message("Found it!")
}

#' a super class to inherit from
#' @export
super_class <-
  R6::R6Class(
    classname = "super_class",
    public    =
      list(
        super_class_action =
          function(){
            my_function()
          }
      )
  )


#' a sub_class that inherits
#' @export
sub_class <-
  R6::R6Class(
    classname = "sub_class",
    inherit   = super_class,
    parent_env = parent.frame(),
    public    =
      list(
        sub_class_action =
          function(){
            my_function()
          }
      )
  )
