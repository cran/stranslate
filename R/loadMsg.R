#' loadMsg
#'
#' Loads translated messages from one (or more) file into package environment. For details read the vignette `vignette("stranslate")`.
#'
#' @param files character: names of text file(s) with translated messages
#' @param .domain character: domain namesd (default: `getOption("stranslate.domain")`)
#' @param .lang character: default language (default: 'en')
#' @param .silent logical: should outputs be displayed during the loading process and then `listMsg()` be called?
#' @param .overwrite logical: should keys be overwritten (default: `FALSE`)
#' 
#'
#' @return invisibly the current message environment
#' @export
#'
#' @examples
#' # note "messages/messages.txt" contains only english and german ;)
#' loadMsg(system.file("messages/messages.txt", package="stranslate"), .overwrite=TRUE) # avoid warning
#' # english
#' getMsg(ROUND=0, .lang="en")
#' getMsg(ROUND=1, .lang="en")
#' getMsg(ROUND=2, .lang="en")
#' # english
#' getMsg(ROUND=0, .lang="de")
#' getMsg(ROUND=1, .lang="de")
#' getMsg(ROUND=2, .lang="de")
#' # default language or if not available then english 
#' getMsg(ROUND=0)
#' getMsg(ROUND=1)
#' getMsg(ROUND=2)
loadMsg <- function(files, .domain=getOption("stranslate.domain"), .lang='en', .silent=TRUE, .overwrite=FALSE) {
  if (!isName(.domain)) stop(sprintf("Only letters, digits, dot and underscores allowed in domain '%s'.", .domain))
  if (is.null(msg[[.domain]])) msg[[.domain]] <- list()
  # browser()
  key <- ' '
  msg$.domain <- .domain
  for (file in files) {
    if (!.silent) cat(" Read file:", file, "\n")
    if (!file.exists(file)) stop(sprintf("File '%s' not found", file))
    suppressWarnings({
      lines <- readLines(files)
    })
    lines <- lines[nchar(lines)>0]          # delete empty lines
    lines <- lines[!startsWith(lines, '#')] # delete comment lines
    lind  <- startsWith(lines, "<")
    lang  <- trimws(c(.lang, substring(lines[lind], 2)))
    for (l in lang) {
      if(is.null(msg[[.domain]][[l]])) msg[[.domain]][[l]] <- list()
    }
    tok1 <- sub("\\s.*", "", lines)
    msgi <- NULL
    msg$.lang <- .lang
    i    <- 1
    while(i<=length(lines)) {
      if (tok1[i]=="<") {
        setLang(trimws(substring(lines[i], 2)), .domain=msg$.domain)
        if (!.silent) cat(getMsg(LANGUAGE=msg$.lang, .domain="stranslate"), "\n")
      } else {
        stopifnot(substr(lines[i], 1, 1) %in% c(letters, LETTERS))
        # get key
        key   <- tok1[i]
        if (!isName(key)) stop(sprintf("Only letters, digits, dot and underscores allowed in key '%s'.", key))
        msgi  <- list(.silent=.silent, .overwrite=.overwrite)
        query <- key
        msgi[[query]] <- trimws(substring(lines[i], 1+nchar(key)))
        j      <- i+1
        while (j<=length(lines)) {
          cont <- FALSE
          if (tok1[j]=="") {
            msgi[[query]] <- paste0(msgi[[query]], "\n", trimws(lines[j]))
          } else if (startsWith(tok1[j], '?')) {
            query <- trimws(substring(tok1[j], 2))
            msgi[[query]] <- trimws(substring(lines[j], 2+nchar(query)))
          } else break
          j <- j+1
        }
        #browser()
        do.call('setMsg', msgi)
        i <- j-1
      }
      i <- i+1
    }
  }
  for (d in names(msg)) {
    if (isName(d)) stopifnot('en' %in% names(msg[[d]]))
  }
  if (!.silent) listMsg()
  invisible(msg)
}
