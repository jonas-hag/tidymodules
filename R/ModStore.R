
#' Create a R6 ModStore class
#' 
#' This class is used to create a state container for tidy modules 
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}} with methods for creating and managing tidy modules.
#' @format \code{\link{R6Class}} object.
#' @examples
#' rvalues <- tidymodules::init(input,output,session)
#' observe({
#'   
#'   rvalues$SessionTracking$log2db(
#'     action="access",
#'     param="home",
#'     desc=getOption("deployVersion")
#'   )
#' })
#' @field dbName Path to SQLight DB file. Default to getOption("dbName") or ./.log/log.sqlite
#'        Note that the DB file as well as its parent folder (.log) need to be writable by others 
#'        to properly collect usage logs.
#' @field identity Authentication object \code{\link{Authentication}}
#' @field force Force tracking on localhost, default to false.
#' 
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{new(identity)}}{This method is used to create object of this class with \code{identity} as Authentication object.}
#'   \item{\code{log2db(action,param,desc)}}{This method log the specific action described with the variables action/param/desc into the tracking database.}
#'   \item{\code{getDbName()}}{This method return the path to the SQLite databasse file.}
#'   }
#' @export
ModStore <- R6::R6Class(
  "ModStore",
  public = list(
    initialize = function() {},
    isStored = function(m){
      s <- self$getSession(m)
      mod <- shiny::isolate(s$collection[[m$module_ns]])
      if(is.null(mod))
        return(FALSE)
      else
        return(TRUE)
    },
    getGlobalSession = function(){
      sid <- "global_session"
      self$getSession(sid)
    },
    getSession = function(m){
      shiny::isolate({
        return(private$getS(m))
      })
    },
    getSessions = function(){
      return(private$sessions)
    },
    getMods = function(m){
      s <- self$getSession(m)
      return(s$collection)
    },
    getEdges = function(m){
      s <- self$getSession(m)
      return(s$edges)
    },
    addEdge = function(from,
                       to,
                       mode = "direct", 
                       comment = NA){
      
      
      fromId <- fname <- fport <- ftype <- fclass <- NA
      toId   <- tname <- tport <- ttype <- tclass <- NA
      s <- e <- d <- NULL
      
      isolate({
        
        if(is(to$m,"TidyModule")){
          s <- to$m$getSession()
          e <- self$getEdges(to$m)
          
          toId  <- to$m$module_ns
          tport   <- to$port
          tname   <- to$m$getPortName(to$port,to$type)
          ttype   <- to$type
          tclass  <- "TidyModule"
        }
        
        if(is(from$m,"TidyModule")){
          if(is.null(s)){
            s <- from$m$getSession()
            e <- self$getEdges(from$m)
          }
          
          fromId  <- from$m$module_ns
          fport   <- from$port
          fname   <- from$m$getPortName(from$port,from$type)
          ftype   <- from$type
          fclass  <- "TidyModule"
        
        # Handle tidymodules derived ports    
        }else if(!is.null(attr(from$m,"tidymodules")) &&
           attr(from$m,"tidymodules")){
          mod <- attr(from$m,"tidymodules_operation")
          if(!is.null(mod) && mod == "combine"){
            mode <- mod
            combinedPorts <- reactiveValuesToList(from$m)
            for(key in names(combinedPorts)){
              f <- combinedPorts[[key]]
              comment <- key
              fromId <- attr(f,"tidymodules_module_ns")
              fport <- attr(f,"tidymodules_port_id")
              ftype <- attr(f,"tidymodules_port_type")
              fname <- attr(f,"tidymodules_port_name")
              fclass  <- "TidyModule"
              
              comb_row <- data.frame(
                from  = fromId,
                fclass = fclass,
                fport = fport,
                ftype = ftype,
                fname = fname,
                to    = toId,
                tclass = tclass,
                tport = tport,
                ttype = ttype,
                tname = tname,
                mode  = mode,
                comment = comment
              )
              
              if(is.null(d))
                d <- comb_row
              else
                d <- rbind(d,comb_row)
            }
          }else{
            fromId <- attr(from$m,"tidymodules_module_ns")
            fport <- attr(from$m,"tidymodules_port_id")
            ftype <- attr(from$m,"tidymodules_port_type")
            fname <- attr(from$m,"tidymodules_port_name")
            fclass  <- "TidyModule"
          }
          
        }else if(shiny::is.reactivevalues(from$m)){
          fromId <- unlist(from$m)$imp$.label
          fclass <- "reactivevalues"
        }else if(shiny::is.reactive(from$m)){
          fromId <- attr(from$m,"observable")$.reactId
          comment <- attr(from$m,"observable")$.label
          # support for previous shiny version that don't have reactId (don't work with shiny 1.0.5)
          if(is.null(fromId))
            fromId <- comment
          fclass <- "reactive"
        }else{
          stop("Unknown 'from' entity in addEdge function ",class(from$m),"/n")
        }
        
        if(is.null(d))
          d <- data.frame(
            from  = fromId,
            fclass = fclass,
            fport = fport,
            ftype = ftype,
            fname = fname,
            to    = toId,
            tclass = tclass,
            tport = tport,
            ttype = ttype,
            tname = tname,
            mode  = mode,
            comment = comment
          )
        
        if(is.null(s) || s$sid == "global_session")
          stop("addEdge function error! Module has no session or session is global [",s$sid,"]")
        
        # track update time
        s$updated = Sys.time()
        
        if(length(s$edges)==0)
          s$edges <- d
        else
          s$edges <- rbind(e,d)
      })
    },
    addMod = function(m){
      shiny::isolate({
        s <- private$getS(m)
        ns <- as.character(m$module_ns)
        
        # if(!is.null(s$collection[[ns]]))
        #   stop(paste0("Module namespace ",ns," already stored!"))
        s$collection[[ns]] <- m
        if(!is.null(m$group)){
          g <- as.character(m$group)
          if(is.null(s$g_collection[[g]]))
            s$g_collection[[g]] <- list()
          s$g_collection[[g]][[ns]] <- m
        }
        if(!is.null(m$parent_ns)){
          p <- as.character(m$parent_ns)
          if(is.null(s$n_collection[[p]]))
            s$n_collection[[p]] <- list()
          s$n_collection[[p]][[ns]] <- m
        }
        # track update time
        s$updated = Sys.time()
        # TODO : Do we really need this line below ?  
        s$ns <- c(s$ns, as.character(m$module_ns))
      })
    },
    delMod = function(m){
      # TODO : Implement this function 
    },
    print = function(){
      aid <- private$getAID()
      shiny::isolate({
        str(private$sessions[[aid]]$global_session$collection)
      })
    }
  ),
  private = list(
    sessions = shiny::reactiveValues(),
    sessionExist = function(sid){
      aid <- private$getAID()
      return(
        !is.null(private$sessions[[aid]]) &&
          !is.null(private$sessions[[aid]][[sid]])
      )
    },
    addSession = function(sid){
      aid <- private$getAID()
      if(is.null(private$sessions[[aid]])){
        private$sessions[[aid]] <- reactiveValues()
      }
      
      if(is.null(private$sessions[[aid]][[sid]])){
        private$sessions[[aid]][[sid]] <- shiny::reactiveValues(
          aid = aid,
          path = getwd(),
          sid = sid,
          count = 0,
          created = Sys.time(),
          updated = Sys.time(),
          collection = list(),
          ns = c(),
          edges = data.frame()
        )
      }else{ FALSE }
    },
    getS = function(m){
      sid <- m
      if(is(m,"TidyModule"))
        sid <-  m$getSessionId()
      aid <- private$getAID()
      if(!private$sessionExist(sid))
        private$addSession(sid)
      return(private$sessions[[aid]][[sid]])
    },
    getAID = function(){
      return(digest::digest(getwd(),algo = "md5"))
    }
  )
)