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
  args  <- match.call(expand.dots = FALSE)$...
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  oargs <- args
  #browser()
  for (i in 1:length(args)) {
    if (is.call(oargs[[i]]) || is.name(oargs[[i]])) try(oargs[[i]] <- eval(oargs[[i]]), silent=TRUE)
    oargs[[i]] <- as.character(oargs[[i]])
    if (nargs[i]=='') {
      nargs[i]   <- oargs[i]
      oargs[[i]] <- ''
    }
  }
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
  for (i in 1:length(oargs)) env[[pargs[i]]] <- oargs[[nargs[i]]]
  if (is.null(env[[key]])) stop(getMsg(MISSING_KEYS, lang=.lang, keys=key, .domain="stranslate", .lang=.lang))
  env[[key]](oargs[1])
}
