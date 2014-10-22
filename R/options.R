#' An option manager for R.
#'
#' See \code{\link{register_options}} for examples and usage.
#' 
#' 
#' @docType package
#' @name options-package
NULL


OPTIONREGISTER <- new.env()

#' Register an options set
#'
#' The function \code{register_options} sets up a new options register. It returns
#' a function that can be uset to set, get, or reset options; or to return a reference to 
#' the options register or to clone the options reference alltogether.
#'
#' The following terms are arguments of the function that is returned and cannot be used as
#' option names:
#' 
#' \code{.where} \code{.reset} \code{.clone} \code{.ref}
#'
#'
#' @param name name of option set
#' @param defaults list of default options
#'
#' @return A \code{function} that can be used as a custom options manager. It has the
#' following arguments.
#' \tabular{ll}{
#' \code{...} \tab Comma separated list of option names (\code{character}) to retrieve options or \code{[name]=[value]} pairs to set options.\cr
#' \code{.where} \tab (\code{environment}; \code{NULL}) Where to set options (optional, default is global) \code{environment} that was obtained using the \code{clone} or \code{ref} option. \cr
#'  \code{.reset} \tab (\code{logical}; \code{FALSE}) Reset options to default values\cr
#'  \code{.clone} \tab (\code{logical}; \code{FALSE}) Create a copy of the options (returns newly created environment)\cr
#'  \code{.ref} \tab  (\code{logical}; \code{FALSE}) Return a reference to the option environment.
#' }
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
#' # When multiple options are retrieved, the result is a list
#' my_options('foo','baz')
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
#' @seealso \code{\link{reset}}, \code{\link{clone_and_merge}}
#' @export
register_options <- function(name,defaults=list()){
  reg <- function(n,d){
    if (name %in% ls(OPTIONREGISTER) ) 
      warning(sprintf("Overwriting pre-existing option register '%s'",name))
    OPTIONREGISTER[[name]] <- new.env()
    OPTIONREGISTER[[name]]$options <- defaults
    OPTIONREGISTER[[name]]$defaults <- defaults
    OPTIONREGISTER[[name]]
  }
  op <- reg(name,defaults)
  
  
  function(..., .where=NULL, .reset=FALSE, .clone=FALSE, .ref=FALSE){
    L <- list(...)
    # global settings if no local setting is pointed to.
    if ( is.null(.where) ) .where <- op
    # are we setting or getting?
    set <- .reset || ( length(L) > 0 && (length(names(L)) == length(L) & !.clone & !.ref) )
    get <- ( length(L) > 0 && is.null(names(L)) ) ||
           ( length(L) == 0 && !.clone && !.ref )
    if (set)    return( invisible(set_option(.where, L, .reset)) )
    if (get)    return( get_option(.where, L) )
    if (.clone)  return( as.environment(as.list(.where)) )
    if (.ref)    return( .where )
    stop('Illegal command')
  }
}

get_option <- function(where, opts){
  if (length(opts) == 0 ) opts <- names(where$options)
  if (length(opts) == 1) 
    where$options[[unlist(opts)]] 
  else 
    where$options[unlist(opts)]
}

set_option <- function(where, opts, reset){
  if (reset){
    where$options <- where$defaults
  } else {
    where$options[names(opts)] <- opts
  }
  where$options
}



clear_optionregister <- function() rm(list=ls(OPTIONREGISTER),envir=OPTIONREGISTER)

#' Manipulate option set
#'
#' @param options A function as returned by \code{\link{register_options}}
#' @param ... Options to be merged. 
#'
#' @return A function like \code{options}. However, the \code{.where} argument cannot be specified
#'   as this function only stores, (re)sets and gets locally options.
#'   
#' @seealso \code{\link{register_options}}
#' @export 
clone_and_merge <- function(options,...){
  op <- options(.clone=TRUE)
  options(..., .where=op)
  # This explicit construction prevents nested calls when clone_and_merge is called
  # repeatedly and iteratively on an option structure. 
  f <- function(...,.where=NULL,.reset=FALSE,.clone=FALSE,.ref=FALSE){}
  body(f) <- body(options)
  f
}

#' Reset options to default values
#' @rdname clone_and_merge
#' @export 
reset <- function(options) options(.reset=TRUE)

#' List of reserved words
#' 
#' Returns a list of words that may not be used as option name.
#' 
#' @export
reserved <- function() c(".where",".clone",".ref",".reset")






