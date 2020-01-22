
#' Utility Module
#' 
#' This is a utility module used by some functions to retrieve the ModStore
#'
#' @docType class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' 
UtilityModule <- R6::R6Class(
  "UtilityModule",
  inherit = TidyModule,
  public = list(
    initialize = function(){
      if(is.null(private$shared$store))
        private$shared$store <- ModStore$new()
      
      
      #### Try to capture server function arguments #######
      for(i in 1:10){
        serverEnv <- parent.env(parent.frame(i))
        if(!is.null(serverEnv)){
          if(!is.null(serverEnv$input) &&
             is(serverEnv$output, "shinyoutput")){
            private$shiny_input <- serverEnv$input
            private$shiny_output <- serverEnv$output
            private$shiny_session <- serverEnv$session
            
            break
          }
        }
      }
      if(is.null(private$shiny_output)){
        serverEnv <- parent.frame(3)
        private$shiny_input <- serverEnv$input
        private$shiny_output <- serverEnv$output
        private$shiny_session <- serverEnv$session
      }
    }
  )
)