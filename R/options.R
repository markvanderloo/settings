#' An option manager for R.
#'
#' See \code{\link{register_options}} for examples and usage.
#'
#' 
#' @docType package
#' @name options-package
NULL


OPTIONREGISTER <- new.env()

#' Register an option set
#'
#' The function \code{register_options} sets up a new options register. It returns
#' a function that can be uset to set, get, or reset options; or to return a reference to 
#' the options register or to clone the options reference alltogether.
#'
#' @param name name of option set
#' @param defaults list of default options
#' 
#' @examples
#' # create an options register
#' my_options <- register_options('myopt',defaults=list(foo=1,bar=2,baz='bob'))
#' 
#' ### Retrieving global options
#' my_options() # retrieve the full options list.
#' my_options('baz')
#' my_options('foo')
#' 
#' # When multiple options are retrieved, the result is simplified unless specified otherwise:
#' my_options('foo','baz')
#' my_options('foo','baz',simplify=FALSE)
#' 
#' ### Setting global options
#' my_options(foo=3,baz='pete')
#' my_options()
#' 
#' ### Create a clone of global options and locally change settings
#' op <- my_options(clone=TRUE)
#' ## Retrieving local options
#' my_options(where=op)
#' my_options('foo','baz',where=op)
#' 
#' ## Setting local options
#' my_options(foo=12,where=op)
#' my_options('foo',where=op)
#' # check that global options are unaltered
#' my_options('foo')
#' 
#' ### Create a reference to (global) options
#' op_ref <- my_options(ref=TRUE)
#' my_options(foo=0,where=op_ref)
#' my_options('foo',where=op_ref) #warning: this changes global settings!
#' my_options('foo')
#' 
#' @export
register_options <- function(name,defaults=list()){
  reg <- function(n,d){
    if (name %in% ls(OPTIONREGISTER) ) 
      warning(sprintf("Overwriting pre-existing option register '%s'",name))
    OPTIONREGISTER[[name]] <- new.env()
    OPTIONREGISTER[[name]]$options <- defaults
    OPTIONREGISTER[[name]]
  }
  op <- reg(name,defaults)
  
  get_option <- function(where, opts, simplify){
    if (length(opts) == 0 ) 
      where$options 
    else
      if (simplify) 
        sapply(opts, function(x) where$options[[x]]) 
      else 
        where$options[unlist(opts)]
  }
  
  set_option <- function(where, opts, reset){
    if (reset){
      where$options <- defaults
    } else {
      where$options[names(opts)] <- opts
    }
    where$options
  }
  
  function(..., .where=NULL, .simplify=TRUE, .reset=FALSE, .clone=FALSE, .ref=FALSE){
    L <- list(...)
    # global settings if no local setting is pointed to.
    if ( is.null(.where) ) .where <- op
    # are we setting or getting?
    set <- .reset || ( length(L) > 0 && (length(names(L)) == length(L) & !.clone & !.ref) )
    get <- ( length(L) > 0 && is.null(names(L)) ) ||
           ( length(L) == 0 && !.clone && !.ref )
    if (set)    return( invisible(set_option(.where, L, .reset)) )
    if (get)    return( get_option(.where, L, .simplify) )
    if (.clone)  return( as.environment(as.list(.where)) )
    if (.ref)    return( .where )
    stop('Illegal command')
  }
}

clear_optionregister <- function() rm(list=ls(OPTIONREGISTER),envir=OPTIONREGISTER)


