#' getMsg
#'
#' Returns a message. The first parameter must be the key to the message. For details read the vignette `vignette("stranslate")`.
#'
#' @param ... parameter(s) given to the function
#' @param .domain character: domain name (default: `getOption("stranslate.domain")`)
#' @param .lang character: language to use (default: `getOption("stranslate.lang")`)
#'
#' @return the (translated) message
#' @importFrom utils adist
#' @export
#'
#' @examples
#' # without a parameter
#' getMsg("DOMAIN_UNIQUE", .domain="stranslate", .lang="en")
#' getMsg('DOMAIN_UNIQUE', .domain="stranslate", .lang="de")
#' getMsg(DOMAIN_UNIQUE, .domain="stranslate")
#' # with a parameter
#' getMsg(LANGUAGE="english", .domain="stranslate", .lang="en")
#' getMsg(LANGUAGE="deutsch", .domain="stranslate", .lang="de")
#' # which system language is used?
#' getMsg(LANGUAGE=Sys.getenv("LANG"), .domain="stranslate")
getMsg <- function (..., .domain=getOption("stranslate.domain"), .lang=getOption("stranslate.lang")) {
  #browser()
  if (length(.domain)!=1) stop(getMsg('DOMAIN_UNIQUE', .domain="stranslate"))
  arg_list <- lapply(substitute(...()), function(e) {
    ee <- try(eval(e), silent=TRUE)
    if (inherits(ee, 'try-error')) e else ee
  })
  nargs <- names(arg_list)
  if (is.null(nargs)) nargs <- rep('', length(arg_list))
  for (i in seq_along(arg_list)) {
    if (nargs[i]=='') {
      nargs[i] <- if(is.character(arg_list[[i]])) arg_list[[i]] else paste0(deparse(arg_list[[i]], width.cutoff = 500L), collapse="")
      arg_list[[i]] <- ''
    }
  }
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
