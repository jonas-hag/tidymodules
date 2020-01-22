
#' Create a R6 Module class
#' 
#' This is the main class provided by tidymodules and should be the parent of all tidymodules modules.
#' It provides methods for defining input and output communication ports, assigning them and managing module namespace.
#'
#' @docType class
#' @return Object of \code{\link{R6Class}} with methods for creating and managing tidymodules modules.
#' @format \code{\link{R6Class}} object.
#' @examples
#' rvalues <- ava::init(input,output,session)
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
#'   \item{\code{getDbName()}}{This method return the path to the SQLite database file.}
#'   }
#' @export
TidyModule <- R6::R6Class(
  "TidyModule",
  public = list(
    id = NULL,
    module_ns = NULL,
    parent_ns = NULL,
    parent_mod = NULL,
    parent_ports = NULL,
    group = NULL,
    created = NULL,
    o = NULL,
    i = NULL,
    initialize = function(id = NULL, inherit = TRUE, group = NULL) {
      
      #### Initialize ModStore #######
      if(is.null(private$shared$store))
        private$shared$store <- ModStore$new()
      
      self$parent_ports <- inherit
      
      ses <- self$getSession()
      isolate({ses$count <- ses$count+1})
      shiny::onStop(function(){ 
        # reset session module count and edge table when app stop 
        ses$count <- 0
        ses$edges <- data.frame()
      })
      
      self$id <- ifelse(
        is.null(id),
        paste0(class(self)[[1]],"-",isolate({ses$count }))
        ,id)
      
      if(!is.null(group)){
        self$id <- paste0(group,"-",self$id)
        self$group <- group
      }
      
      ################# Handle nested module here ################
      ############### Find and set the parent module #############
      
      # case 1 : nested module directly initialized in attribute definition of parent
      # This type of initialization should trigger an error
      # Because the parent module need to be initialized first
      # TODO : Find a way to prevent this....
      
      # if(class(sys.frame(9)$inherit) == "R6ClassGenerator")
      #   stop(paste0("Error in definition of module ",sys.frame(9)$classname,
      #              "! Nested module ",self$id," should be defined either in initialize() or server() of parent!"))
      
      # case 2a : nested module initialized in initialze() or server() functions of parent
      if(is.null(self$parent_mod))
        self$parent_mod<-parent.env(parent.frame(3))$self
      
      # case 2b : same as 2a but support nested functions or module inheritance
      for(n in 4:10)
        if(is.null(self$parent_mod) || !is(self$parent_mod,"TidyModule")){
          self$parent_mod<-parent.env(parent.frame(n))$self
          break
        }
      
      # case 3 : Dynamically created module in an observe function of server()
      if(is.null(self$parent_mod) || !is(self$parent_mod,"TidyModule"))
        self$parent_mod<-parent.env(parent.env(parent.frame(3)))$self
      
      # At the end we save the parent ns & server arguments if any
      if(!is.null(self$parent_mod) && 
         is(self$parent_mod,"TidyModule")){
        self$parent_ns <- self$parent_mod$module_ns
        private$shiny_input   <- self$parent_mod$getShinyInput()
        private$shiny_output  <- self$parent_mod$getShinyOutput()
        private$shiny_session <- self$parent_mod$getShinySession()
      }else{
        self$parent_mod <- NULL
      }
      
      self$module_ns <- ifelse(
        is.null(self$parent_ns),
        self$id,
        paste0(self$parent_ns,"-",self$id))
      
      #### Try to capture server function arguments if not set #######
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
      
      # check that the module namespace is unique
      # if(self$isStored())
      #   stop(paste0("Module namespace collision with ",self$module_ns,", is it already used?"))
      
      self$created <- Sys.time()
      private$shared$store$addMod(self)
      private$initFields()
      
      # Only transfer parent ports in dynamic mode
      # i.e. when the child module is initialized on the server
      if(!self$isGlobal() &&
         !is.null(self$parent_mod) && 
         is(self$parent_mod,"TidyModule") &&
         self$parent_ports)
        self$parent_mod %:i:% self
    },
    ns = function(id){
      shiny::NS(self$module_ns, id)
    },
    getSessionId = function(){
      return(getSessionId(private$shiny_output))
    },
    render = function(...){
      self$ui(...)
    },
    ui = function(){
      return(shiny::tagList())
    },
    server = function(input, 
                      output, 
                      session){
      # Need to isolate this block to avoid unecessary triggers
      shiny::isolate({
        private$shiny_session <- session
        private$shiny_input <- input
        private$shiny_output <- output
      })
    },
    definePort = function(x){
      shiny::isolate(x)
    },
    assignPort = function(x){
      shiny::observe({
        shiny::isolate(x)
      })
    },
    addInputPort = function(
      name = NULL,
      description = NULL,
      sample = NULL,
      input = FALSE,
      is_parent = FALSE){
      private$addPort(
        type = "input",
        name = name,
        description = description,
        sample = sample,
        is_parent = is_parent
      )
    },
    updateInputPort = function(
      id = NULL,
      input = NULL){
      private$updatePort(id = id,port = input, type = "input")
    },
    updateInputPorts = function(
      inputs = NULL){
      private$updatePorts(ports = inputs, type = "input")
    },
    getInputPort = function(id = 1){
      return(private$getPort(id,"input"))
    },
    getInputPorts = function(){
      return(private$getPorts("input"))
    },
    getInput = function(id = 1){
      return(private$get(id, "input"))
    },
    updateOutputPort = function(
      id = NULL,
      output = NULL){
      private$updatePort(id = id,port = output, type = "output")
    },
    updateOutputPorts = function(
      outputs = NULL){
      private$updatePorts(ports = outputs, type = "output")
    },
    addOutputPort = function(
      name = NULL,
      description = NULL,
      sample = NULL,
      output = FALSE,
      is_parent = FALSE){
      private$addPort(
        type = "output",
        name = name,
        description = description,
        sample = sample,
        is_parent = is_parent
      )
    },
    getPortName = function(id = NULL, type = "input"){
      port <- private$getPort(id,type)
      return(port$name)
    },
    getOutputPort = function(id = 1){
      return(private$getPort(id,"output"))
    },
    getOutputPorts = function(){
      return(private$getPorts("output"))
    },
    getOutput = function(id = 1){
      return(private$get(id, "output"))
    },
    getStore = function(){
      return(private$shared$store)
    },
    countInputPort = function(){
      return(private$countPort("input"))
    },
    countOutputPort = function(){
      return(private$countPort("output"))
    },
    use = function(...){
      self$callModule(...)
    },
    callModule = function(...){
      # arguments from server environment
      output <- parent.frame()$output
      input <- parent.frame()$output
      session <- parent.frame()$session
      disable_cache <- getCacheOption()
      
      if(!is.null(private$shiny_output)){
        self$doServer(...)
      }else{
        mod <- self$deepClone(output,input,session)
        
        isolate({
          currentSession <- mod$getSession()
          globalSession <- self$getGlobalSession()
          currentSession$edges <- data.frame()
          currentSession$count <- globalSession$count
        })
        
        if(!mod$isStored() || disable_cache){
          self$getStore()$addMod(mod)
          mod$doServer(...)
        }else{
          remove(mod)
          getMod(self$module_ns)$doServer(...)
        }
      }
    },
    isStored = function(){
      return(self$getStore()$isStored(self))
    },
    isGlobal = function(){
      if(self$getSessionId() == "global_session")
        return(TRUE)
      else
        return(FALSE)
    },
    getSession = function(){
      return(self$getStore()$getSession(self))
    },
    getGlobalSession = function(){
      return(self$getStore()$getGlobalSession())
    },
    doServer = function(...){
      shiny::callModule(self$server,self$id,...)
    },
    getPortDef = function(type = NULL, id = 1){
      port <- NULL
      if(is.null(type) || !type %in% c("input","output"))
        stop("type must be one of input/output")
      
      shiny::isolate({
        port <- private$getPort(id,type)
        shiny::reactiveValuesToList(port)
      })
      
    },
    print = function(){
      shiny::isolate({
        cat(paste0("Module Namespace ",self$module_ns,"\n"))
        cat(paste0("Module Session ",self$getSessionId(),"\n"))
        cat(paste0("- Class ",paste0(class(self),collapse = " << "),"\n"))
        cat(paste0("- Input [",self$countInputPort(),"]\n"))
        private$printPorts("input")
        cat(paste0("- Output [",self$countOutputPort(),"]\n"))
        private$printPorts("output")
      })
    },
    deepClone = function(o = NULL, i = NULL, s = NULL){
      isolate({
        copy <- self$clone()
        copy$reset(o,i,s)
        
        for(type in c("input","output")){
          if(private$countPort(type) > 0)
            for(idx in 1:private$countPort(type)){
              po <- private$getPort(idx,type)
              # Don't add port inherited from parent module
              # They will be added by parent, see below...
              if(po$parent)
                next
              if(type == "input")
                copy$addInputPort(po$name,po$description,po$sample)
              else
                copy$addOutputPort(po$name,po$description,po$sample)
            }
        }
        
        # Now deep clone the module attributes that are TidyModules, i.e. nested modules
        # TODO : Add code to check list as well
        for(at in names(self)){
          classes <- class(self[[at]])
          if(length(classes) > 1 && 
             rev(classes)[[2]] == "TidyModule" &&
             at != "parent_mod"){
            copy[[at]] <- self[[at]]$deepClone(o,i,s)
            copy[[at]]$parent_mod <- copy
            # Now add ports to child if any
            if(copy[[at]]$parent_ports)
              copy %:i:% copy[[at]]
            self$getStore()$addMod(copy[[at]])
          }
        }
        
        copy$created <- Sys.time()
      })
      
      return(copy)
    },
    reset = function(o = NULL, i = NULL, s = NULL){
      private$input_port <- reactiveValues()
      private$output_port <- reactiveValues()
      private$port_names <- reactiveValues()
      if(!is.null(o))
        private$shiny_output <- o
      if(!is.null(i))
        private$shiny_input <- i
      if(!is.null(s))
        private$shiny_session <- s
    },
    getShinyInput = function(){
      return(private$shiny_input)
    },
    getShinyOutput = function(){
      return(private$shiny_output)
    },
    getShinySession = function(){
      return(private$shiny_session)
    }
  ),
  private = list(
    shared = { e <- new.env(); e$store <- NULL ; e },
    shiny_input = NULL,
    shiny_output = NULL,
    shiny_session = NULL,
    input_port = NULL,
    output_port = NULL,
    port_names = NULL,
    initFields = function(){
      private$input_port <- shiny::reactiveValues()
      private$output_port <- shiny::reactiveValues()
      private$port_names <- shiny::reactiveValues()
    },
    countPort = function(type = "input"){
      key = paste0(type,"_port")
      return(length(names(private[[key]])))
    },
    addPort = function(
      type = "input",
      name = NULL,
      description = paste0("Short description for this module's ",type),
      sample = NULL,
      port = FALSE,
      is_parent = FALSE){
      stopifnot(!is.null(name) && !is.null(sample))
      rv <- shiny::reactiveValues(
        name = name,
        description = description,
        sample = sample,
        parent = is_parent
      )
      
      p = port
      attr(p,"tidymodules")  <- TRUE
      attr(p,"tidymodules_port_type")        <- type
      attr(p,"tidymodules_port_id")          <- private$countPort(type)+1
      attr(p,"tidymodules_port_name")        <- name
      attr(p,"tidymodules_port_description") <- description
      attr(p,"tidymodules_port_sample")      <- sample
      attr(p,"tidymodules_is_parent")        <- is_parent
      attr(p,"tidymodules_module_ns")        <- self$module_ns
      
      rv[["port"]] <- p
      private[[paste0(type,"_port")]][[name]] <- rv
      nv <- private$port_names[[type]]
      private$port_names[[type]] <- c(nv,name)
    },
    updatePort = function(id = NULL, port = NULL, type = "input"){
      stopifnot((!is.null(id)))
      
      if(!is.null(port)){
        if(!shiny::is.reactivevalues(port) &&
           !shiny::is.reactive(port) )
          stop(paste0(deparse(substitute(port))," is not reactive"))
        
        key = paste0(type,"_port")
        if(is.numeric(id)){
          if(!id %in% seq(1,private$countPort(type))){
            stop(paste0("Port Update Failure: Numeric ",type," port [",id,"] not found in Module definition"))
          }else{
            id<-private$port_names[[type]][id]
          }
        }
        if(is.character(id) && !id %in% names(private[[key]])){
          stop(paste0("Port Update Failure: Character ",type," port [",id,"] not found in Module definition"))
        }
        
          
        # Attach module information to the port
        # This will facilitate storage of module edges
        isolate({
          attrs <- attributes(private[[key]][[id]][["port"]])
          for(a in names(attrs))
            if(grepl("tidymodules",a))
              attr(port,a) <- attrs[[a]]
            
            private[[key]][[id]][["port"]] <- port
        })
      }
    },
    updatePorts = function(ports = NULL, type = "input"){
      stopifnot(!is.null(ports))
      if(!shiny::is.reactivevalues(ports))
        stop(paste0(deparse(substitute(ports))," is not a reactive expression"))
      
      key = paste0(type,"_port")
      
      isolate({
        for(p in names(ports)){
          if(p %in%  private$port_names[[type]]){
            stop(paste0("Adding port name ",p," failed, it already exist in ",type," port definition."))
          }else{
            port <- ports[[p]]
            private$addPort(type,port$name,port$description,port$sample,port$port,is_parent = TRUE)
          }
        }
      })
    },
    get = function(id = 1,type = "input"){
      data.port <- private$getPort(id, type)
      
      shiny::req(data.port)
      # shiny::req(data.port[[type]])

      return(data.port[["port"]])
    },
    getPort = function(id = 1,type = "input"){
      key = paste0(type,"_port")
      if(private$countPort(type) == 0){
        warning(paste0("Module ",self$module_ns," has no ",type," ports"))
        return(NULL)
      }
      if(is.numeric(id)){
        if(!id %in% seq(1,private$countPort(type))){
          warning(paste0("Numeric ",type," port [",id,"] not found in Module definition"))
          return(NULL)
        }else{
          id<-private$port_names[[type]][id]
        }
      }
      if(is.character(id) && !id %in% names(private[[key]])){
        warning(paste0("Character ",type," port [",id,"] not found in Module definition"))
        return(NULL)
      }
      return(private[[key]][[id]])
    },
    getPorts = function(type = "input"){
      key = paste0(type,"_port")
      if(private$countPort(type) == 0)
        return(NULL)

      return(private[[key]])
    },
    printPorts = function(type = "input"){
      if(private$countPort(type)>0)
        for (p in 1:private$countPort(type)) {
          port = private$getPort(p,type)
          cat(paste0("(",p,") ",port$name," => ",ifelse(
            is.reactivevalues(port[["port"]]) ||
            is.reactive(port[["port"]]) || 
            port[["port"]],"OK","Empty"),"\n"))
        }
    },
    trimParentNS = function(){
      if(!is.null(self$parent_ns)){
        return(
          sub(
            paste0(self$parent_ns,"-"),
            "",
            self$module_ns
          )
        )
      } else {
        return(self$module_ns)
      }
    }
  )
)
