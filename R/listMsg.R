#' listMsg
#'
#' Lists all keys in all languages in a specific domain or all domains (default).
#'
#' @param .domain character: domains(s) to list (default: `NULL`)
#'
#' @return nothing
#' @importFrom crayon red
#' @export
#'
#' @examples
#' listMsg("stranslate")
#' listMsg()
#' # load some more text
#' loadMsg(system.file("messages/messages.txt", package="stranslate"), .overwrite=TRUE) # avoid warning
#' listMsg()
listMsg <- function(.domain=NULL) {
  #browser()
  domain <- getOption("stranslate.domain")
  lang   <- getOption("stranslate.lang")
  msgcat <- c('',
              getMsg(CURRENT_DOMAIN=domain, .domain="stranslate"),
              paste(getMsg(CURRENT_LANGUAGE=lang, .domain="stranslate"), "->", paste0(language(lang), collapse=", "))
  )
  if (is.null(.domain)) .domain <- names(msg)
  allfunctions <- allFunctions()
#  warn <- NULL
  for (d in .domain) {
    if (isName(d)) {
      msgcat <- c(msgcat, '', getMsg(DOMAIN=d, .domain="stranslate"))
      if (!is.null(msg[[d]])) {
        #browser()
        keys.defined <- keys.used <- list()
        for (l in names(msg[[d]])) {
          #browser()
          msgcat <- c(msgcat, getMsg(LANGUAGE=l, .domain="stranslate"))
          keys.defined[[l]] <- names(msg[[d]][[l]])
          kb <- NULL
          for (k in keys.defined[[l]]) {
            matches <- environment(msg[[d]][[l]][[k]])$matches
            kb      <- c(kb, unlist(lapply(matches, function(v) { v[,2]})))
          }
          keys.used[[l]] <- setdiff(all.names(parse(text=kb)), allfunctions)
          keyl           <- setdiff(keys.used[[l]], keys.defined[[l]])
          allkeys        <- c(keys.defined[[l]], red(keyl))
          lkeys <- cumsum(2+nchar(allkeys))%/%(getOption("width")-10)
          for (i in 0:max(lkeys)) msgcat <- c(msgcat, paste("    ", paste0(allkeys[lkeys==i], collapse=", ")))
#          if (length(keyl)) warn <- c(warn, getMsg(UNDEFINED_KEYS=paste0(keyl, collapse=", "), lang=l, .domain="stranslate"))
        }
        #
        allkeys <- setdiff(unique(unlist(keys.defined)), '')
        for (l in names(msg[[d]])) {
          keyl <- setdiff(allkeys, names(msg[[d]][[l]]))
          if (length(keyl)) msgcat <- c(msgcat, red(getMsg(UNDEFINED_KEYS=paste0(keyl, collapse=", "), .domain="stranslate")))
#          if (length(keyl)) warn <- c(warn, getMsg(UNDEFINED_KEYS=paste0(keyl, collapse=", "), lang=l, .domain="stranslate"))
        }
      }
    }
  }
  cat(paste0(msgcat, "\n"))
#  if (length(warn)) {
#    warn <- unique(warn)
#    for (warni in warn) warning(warni)
#  }
}
