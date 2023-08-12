#' getMsg
#'
#' Returns a message. The first parameter must be the key to the message. For details read the vignette `vignette("stranslate")`.
#'
#' @param ... parameter(s) given to the function
#' @param .domain character: domain namesd (default: `getOption("stranslate.domain")`)
#' @param .lang character: language to use (default: `getOption("stranslate.lang")`)
#'
#' @return the (translated) message
#' @importFrom utils adist
#' @export
#'
#' @examples
#' 1+1
#'
getMsg <- function (..., .domain=getOption("stranslate.domain"), .lang=getOption("stranslate.lang")) {
  if (length(.domain)!=1) stop(getMsg('DOMAIN_UNIQUE', .domain="stranslate"))
  # Extract the arguments from the function call
  #args  <- match.call(expand.dots = FALSE)$...
  #nargs <- names(args)
  #if (is.null(nargs)) nargs <- rep('', length(args))
  #oargs <- args
  ##browser()
  #for (i in 1:length(args)) {
  #  if (is.call(oargs[[i]]) || is.name(oargs[[i]])) try(oargs[[i]] <- eval(oargs[[i]]), silent=TRUE)
  #  oargs[[i]] <- as.character(oargs[[i]])
  #  if (nargs[i]=='') {
  #    nargs[i]   <- oargs[[i]]
  #    oargs[[i]] <- ''
  #  }
  #}
  arg_list <- substitute(list(...))[-1]
  nargs    <- names(arg_list)
  if (is.null(nargs)) nargs <- rep('', length(arg_list))
  nnargs   <- nchar(nargs)
  for (i in seq_along(arg_list)) {
    err <- try(arg_list[[i]] <- eval(arg_list[[i]], envir=parent.frame()), silent=TRUE)
    if (inherits(err, 'try-error')) arg_list[[i]] <- as.character(arg_list[[i]])
    if (nnargs[i]==0) {
      nargs[i] <- as.character(arg_list[[i]])
      arg_list[[i]] <- ''
    }
  }
  arg_list        <- as.list(arg_list)
  names(arg_list) <- nargs
  key    <- nargs[1]
  #
  lang   <- language(.lang, .domain=.domain)
  env    <- NULL
  for (l in lang) {
    result <- msg[[.domain]][[l]][[key]]
    if (!is.null(result)) {
      env <- msg[[.domain]][[l]]
      break
    }
  }
  stopifnot(!is.null(env))
  #
  pargs <- paste0('.', nargs)
  for (i in seq_along(arg_list)) env[[pargs[i]]] <- arg_list[[nargs[i]]]
  if (is.null(env[[key]])) stop(getMsg(MISSING_KEYS=key, lang=.lang, .domain="stranslate", .lang=.lang))
  env[[key]](arg_list[[1]])
}
