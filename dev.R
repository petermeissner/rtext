R6ext <-
  R6::R6Class(
    #### class name ==============================================================
    "R6ext",
    private = list(

    ),
    public = list(
      #### [ verbose ] #### ....................................................
      verbose = TRUE,
      #### [ get() ] #### ......................................................
      get = function(name){
        if(name=="private"){
          return(private)
        }
        if( name %in% names(self) ){
          return(get(name, envir=self))
        }else if( name %in% names(private) ){
          return(get(name, envir=private))
        }else{
          return(NULL)
        }
      },

      #### [ message() ] #### ..................................................
      message = function(x, ...){
        xname <- as.character(as.list(match.call()))[-1]
        if(self$verbose){
          if(is.character(x)){
            message(class(self)[1], " : ", x, ...)
          }else{
            message(class(self)[1], " : ", xname, " : \n", x, ...)
          }
        }
      }
    )
  )


