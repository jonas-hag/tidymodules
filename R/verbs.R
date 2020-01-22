
# 
# Module pipe operator/verbs definition
#

#' connect ports from two modules
#' 
#' @export
connect_ports <- function(leftModule = NULL, leftPort = 1,
                    rightModule = NULL, rightPort = 1,
                    reverse = FALSE){
  if(!is.numeric(leftPort))
    stop("Left port ID 'leftPort' should be numeric")
  if(!is.numeric(rightPort))
    stop("Right port ID 'rightPort' should be numeric")
  
  fct <- mkDoublePipe(leftPort,rightPort,rev = reverse)
  fct(leftModule,rightModule)
}


#' combine ports into a reactiveValues object
#' 
#' @export
combine_ports <- function(...){
  args <- list(...)
  r <- NULL
  if(length(args)){
    if(is.null(names(args)))
      names(args) <- 1:length(args)
    r <- do.call(shiny::reactiveValues,args)
  }else{
    r <- shiny::reactiveValues()
  }
  
  # Make this reactive aware of its tidymoduleness
  attr(r,"tidymodules") <- TRUE
  attr(r,"tidymodules_operation") <- "combine"
  
  return(r)
}

#' collapse ports into a single port and make them race
#' i.e. always return the last one updated
#' 
#' @export
race_ports <- function(racers = NULL, to = NULL, port = NULL){
  
  if(is.null(racers) || is.null(to) || is.null(port))
    stop("some function arguments missing")
  
  p <- length(racers)+1
  r <- reactiveVal(label = "race", value = reactive({ }))
  
  lapply(1:length(racers),function(r){
    reac <- racers[[r]]
    observeEvent({
      reac()
    },{
      shiny::req(reac())
      r(reac)
    },priority = p)
    p <- p - 1
  })
  
  observeEvent(r(),{
    to$updateInputPort(port,r()) 
  },priority = p)
}
