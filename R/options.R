#' An option manager for R.
#'
#' See \code{\link{register_options}} for examples and usage.
#' 
#' 
#' @docType package
#' @name options-package
NULL

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
options_manager <- function(...){
  stop_if_reserved(...)
  .defaults <- list(...)
  .op <- .defaults
  
  function(..., .__defaults=FALSE, .__reset=FALSE){
    L <- list(...)
    if (.__defaults) return(.defaults)
    if (.__reset){
      .op <<- .defaults
      return(invisible(.op))
    }
    # get all options
    if (length(L) == 0) return(.op)
    # set options:
    vars <- names(L)
    if ( !is.null(vars) && !any(vars == "") ){
      if (!all(vars %in% names(.defaults))){
        v <- paste(vars[!vars %in% names(.defaults)],collapse=", ")
        warning(sprintf("Adding options not defined in default: %s",v))
      }
      .op[vars] <<- L
      return(invisible(.op))
    }
    # get options
    if (is.null(vars)){
      vars <- unlist(L)
      return( if (length(vars)==1) .op[[vars]] else .op[vars] )
    }      
    stop("Illegal arguments")
  }
}




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
  df <- options(.__defaults=TRUE)
  op <- options()
  f <- do.call(options_manager,df)
  do.call(f,op)
  f(...)
  f
}

#' Reset options to default values
#' @rdname clone_and_merge
#' @export 
reset <- function(options) options(.__reset=TRUE)

#' Check if an option name is reserved
#' 
#' Utility function for programmers using the options package.
#' 
#' 
#' @section Details:
#' This is a utility function that checks if the keys of the key-value pairs
#' \code{...} contain reserved words. The reserved words are
#' 
#' \code{.__defaults}, \code{.__reserved}.
#' 
#' If reserved words are encountered in the input an error thrown.
#' 
#' @param ... Comma-separated \code{[key]=[value]} pairs
#' 
#' @return \code{logical}, indicating if any of the keys was reserved (invisibly).
#' 
#' @export
stop_if_reserved <- function(...){
  res <- c(".__defaults",".__reserved")
  out <- names(list(...)) %in% res
  if (any(out)){
    v <- paste(names(list(...))[out],collapse=", ")
    stop("Reserved word used as option name: ",v)
  }
  invisible(out)
}

#' Find out if we're setting or getting
#' 
#' @param \code{[key]=[value]} pairs of options
#' @return logical
#' @export 
set_options <- function(...){
  L <- list(...)
  nm <- names(L)
  set <- !is.null(nm) && !any(nm=="")
  get <- length(L)  == 0 | (length(L) > 0 & is.null(nm)) 
  stopifnot(set | get)
  set
}


