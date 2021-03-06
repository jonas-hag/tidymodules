
#' R6 Class Representing a ModStore
#'
#' @description
#' This class is used to create a storage for tidymodules objects.
#'
#' @details
#' Manage applications, sessions and modules.
#' @export
ModStore <- R6::R6Class(
  "ModStore",
  public = list(
    #' @description
    #' Create a new ModStore object.
    #' Should be called once by the TidyModule class.
    #' Not to be called directly outside TidyModule.
    #' The ModStore object can be retrieved from any TidyModule object, see example below.
    #' @examples
    #' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
    #' m <- MyModule$new()
    #' s <- m$getStore()
    #' @return A new `ModStore` object.
    initialize = function() {},
    #' @description
    #' Check if a module is stored in the current session.
    #' @param m TidyModule object.
    #' @examples
    #' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
    #' m <- MyModule$new()
    #' s <- m$getStore()
    #' s$isStored(m)
    isStored = function(m){
      s <- self$getSession(m)
      mod <- shiny::isolate(s$collection[[m$module_ns]])
      if(is.null(mod))
        return(FALSE)
      else
        return(TRUE)
    },
    #' @description
    #' Retrieve the global session 'global_session'.
    #' This is the session that exists outside the application server function 
    getGlobalSession = function(){
      sid <- "global_session"
      self$getSession(sid)
    },
    #' @description
    #' Retrieve a module session.
    #' This could be the global session or a user session.
    #' @param m TidyModule object.
    getSession = function(m){
      shiny::isolate({
        return(private$getS(m))
      })
    },
    #' @description
    #' Retrieve all sessions.
    getSessions = function(){
      return(private$sessions)
    },
    #' @description
    #' Retrieve all modules.
    #' @param m TidyModule object.
    getMods = function(m){
      s <- self$getSession(m)
      return(s$collection)
    },
    #' @description
    #' Retrieve modules connections.
    #' @param m TidyModule object.
    getEdges = function(m){
      s <- self$getSession(m)
      return(s$edges)
    },
    #' @description
    #' Add modules connections into ModStore.
    #' An edge is either a connection between a reactive object and a module 
    #' or between two modules.
    #' @param from list with three elements: m -> module, type -> input or output, port -> port Id.
    #' @param to list with three elements: m -> module, type -> input or output, port -> port Id.
    #' @param mode The type of edge, default to 'direct'.
    #' @param comment Any additional comment.
    addEdge = function(from,
                       to,
                       mode = "direct", 
                       comment = NA){
      
      
      fromId <- fname <- fport <- ftype <- fclass <- NA
      toId   <- tname <- tport <- ttype <- tclass <- NA
      s <- e <- d <- NULL
      
      shiny::isolate({
        
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
            combinedPorts <- shiny::reactiveValuesToList(from$m)
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
    #' @description
    #' Add module into the ModStore.
    #' @param m TidyModule object.
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
    #' @description
    #' Delete a module from the ModStore.
    #' @param m TidyModule object.
    delMod = function(m){
      # TODO : Implement this function 
    },
    #' @description
    #' Print the ModStore object.
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
        private$sessions[[aid]] <- shiny::reactiveValues()
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