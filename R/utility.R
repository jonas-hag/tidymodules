
#' Retrieve module from ModStore
#' 
#' This utility function retrieve tidymodules from the central ModStore
#' using module namespace/id and/or group
#' 
#' @export
getMod <- function(id = 1, group = NULL){
  m <- UtilityModule$new()
  mod <- NULL
  c <- isolate(m$getSession()$collection)
  gc <- isolate(m$getSession()$g_collection)
  
  if(!is.null(group) && !is.numeric(id))
    id <- paste0(id,"-G-",group)
  
  if(is.null(group)){
    mod <- c[[id]]
  }else{
    mod <- gc[[group]][[id]]
  }
  
  if(is.null(mod))
    warning(paste0("Module ",id," not found!"))
  
  mod
}

#' @export
mod <- function(id = 1, g = NULL){
  getMod(id,g)
}

#' @export
port <- function(id = 1, p = 1, t = "in", g = NULL){
  m <- getMod(id,g)
  if(is.null(m)){
    return(NULL)
  }else{
    if(t == "in")
      return(m$getInput(p))
    else
      return(m$getOutput(p))
  }
}

#' @export
ipo <- function(id = 1, p = 1, g = NULL){
  port(id,p,"in",g)
}

#' @export
opo <- function(id = 1, p = 1, g = NULL){
  port(id,p,"out",g)
}

#' @export
getCacheOption <- function(){
  disable_cache = getOption("tm_disable_cache")
  if(is.null(disable_cache))
    disable_cache <- FALSE
  disable_cache <- as.logical(disable_cache)
  
  if(is.na(disable_cache))
    stop("Option 'tm_disable_cache' should be set to a logical value or unset.")
  
  disable_cache
}

#' Retrieve module from ModStore
#' 
#' This utility function retrieve tidymodules from the central ModStore
#' using module namespace/id and/or group
#' 
#' @export
callModules <- function(){
  currentSession <- UtilityModule$new()$getSession()
  globalSession <- UtilityModule$new()$getGlobalSession()
  disable_cache <- getCacheOption()
  
  calls <- c()
  
  isolate({
    # re-initialize current session
    currentSession$edges <- data.frame()
    currentSession$count <- globalSession$count
    
    lapply(globalSession$collection,function(mod){
      if(is.null(currentSession$collection[[mod$module_ns]]) || disable_cache){
        ######## Try to capture server function arguments here ########
        serverEnv <- parent.frame(3)
        o <- i <- s <- NULL
        if(!is.null(serverEnv)){
          if(!is.null(serverEnv$input) &&
             is(serverEnv$input, "reactivevalues"))
            i <- serverEnv$input
          if(!is.null(serverEnv$output) &&
             is(serverEnv$output, "shinyoutput"))
            o <- serverEnv$output
          if(!is.null(serverEnv$session) &&
             is(serverEnv$session, "ShinySession"))
            s <- serverEnv$session
        }
        cloned <- mod$deepClone(o,i,s)
        cloned$getStore()$addMod(cloned)
      }
      # Don't invoke nested modules as they will be invoked by parents
      # TODO : Change function to allow callModules within Module server (inject nested modules)
      if(is.null(currentSession$collection[[mod$module_ns]]$parent_ns))
        calls <<- c(calls,currentSession$collection[[mod$module_ns]])
    })
  })
  lapply(calls,function(m) m$callModule()) 
}

#' @export
session_type <- list(
  SHINY = 1,
  USER  = 2,
  CUSTOM = 3
)

#' @export
getSessionId <- function(out = NULL){
  if(is.null(out)){
    return("global_session")
  } else {
    stype <- getOption("tm_session_type")
    sid <- NULL
    if(is.null(stype))
      stype <- session_type$USER
    switch(stype,
       # SHINY
       {
         sid <- unlist(out)$impl$token
       },
       # USER
       {
         r <- unlist(out)$impl$request
         sid <- paste0(r$REMOTE_ADDR,"@",r$HTTP_HOST,r$PATH_INFO)
       },
       # CUSTOM
       {
         fct <- getOption("tm_session_custom")
         if(is.null(fct) || class(fct) != "function")
           stop("Option 'tm_session_custom' should be set to a function taking a shinyoutput object as option and generating a custom session ID used by tidymodules to identify module sessions.")
         sid <- fct(out)
       }
    )
    return(sid)
  }
}
