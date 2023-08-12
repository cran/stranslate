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
  as_character <- function(object) {
    if (length(object)>1) stop(getMsg('KEY_LENGTH', .domain="stranslate"))
    if (inherits(object, 'call'))       object <- deparse(object)
    if (!inherits(object, 'character')) object <- toString(object)
    object
  }
  template <- function(result, args, .lang, .domain) {
  #  matches <- str_match_all(text, "\\{([^{}]+\\$?[^{}]+)?\\}")[[1]]
  #  if (nrow(matches)) {
  #    keys <- strsplit(matches[,2], '$', fixed=TRUE)
  #    repl <- sapply(keys, function(v)) {
  #      if (length(v)>2) return(paste(v, sep='$'))
  #
  #    }
  #  }
  #


    #targs <- args
    #targs[[key]] <- primary
    #for (l in lang) {
    #  txt     <- msg[[domain]][[l]][[key]]
    #  if (!is.null(txt)) {
    #    matches <- unique(unlist(regmatches(txt, gregexpr("\\{([^\\{\\}]+)\\}", txt)))) # extract templates
    #    extract <- strsplit(gsub("\\{|\\}|\\s", "", matches), "$", fixed=TRUE)
    #    for (i in 1:length(extract)) {
    #      replace <- as_character(targs[[extract[[i]][2]]])
    #      if (nchar(extract[[i]][1])) replace <- template(extract[[i]][1], replace, args, lang, domain)
    #      #browser()
    #      txt <- gsub(matches[i], replace, txt, fixed=TRUE)
    #    }
    #    return(eval(parse(text=txt)))
    #  }
    #}
    #keys <- NULL
    #for (l in names(msg)) keys <- c(keys, names(msg[[.domain]][[l]]))
    #keys <- unique(keys)
    #dkey <- adist(keys, key, ignore.case=TRUE)
    #keys <- keys[order(dkey)]
    #if (length(keys)>5) keys <- keys[1:5]
    #stop(getMsg('KEY_NOT_FOUND', key=key, lang=paste0(lang, collapse=", "),
    #            "\n", paste0("  ", keys, collapse="\n")), .domain="stranslate")
  }
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
  #for (i in 1:length(args)) {
  #  if (is.null(oargs[[as.character(i)]])) oargs[[as.character(i)]] <- oargs[[i]]
  #}
  browser()
  env[[key]](oargs[1])
#  oargs <- list()
#  if (is.null(nargs)) nargs <- rep('', length(args))
#  for (i in 1:length(args)) {
#    if (nchar(nargs[i])) {
#      argi <- args[[i]]
#      if (inherits(argi, c("language", "symbol", "expression", "call", "name"))) argi <- eval(argi)
#      if (inherits(args[[i]], "name")) args[[i]] <- parent.frame()[[as.character(args[[i]])]]
#      if (inherits(args[[i]], "call")) args[[i]] <- eval(args[[i]], envir=parent.frame())
#      oargs[[as.character(i)]] <- oargs[[nargs[i]]] <- as.character(eval(args[[i]]))
#    } else {
#      argi <- as.character(args[[i]])
#      oargs[[as.character(i)]] <- oargs[[argi]] <- ''
#    }
#  }

#    lang   <- language(.lang, .domain=.domain)
#
#  for (l in lang) {
#    result <- msg[[.domain]][[l]][[key]]
#    if (!is.null(result)) return(template(result, oargs, .domain=.domain, .lang=.lang))
#  }
#  stop(getMsg(MISSING_KEYS, lang=.lang, keys=key, .domain="stranslate", .lang=.lang))
}
