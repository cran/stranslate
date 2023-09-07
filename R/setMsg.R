#' setMsg
#'
#' Sets for a key the default message and other optional messages. The first argument specifies the key and the default message. 
#' Further named arguments give the message if the key is the same as the name,
#'
#' @param ... first argument is a key and the default message, further named arguments give optional messages
#' @param .silent logical: should the key shown during the process
#'
#' @return returns invisibly the key
#' @importFrom stringr str_match_all
#' @export
#'
#' @examples
#' setLang("de", .domain="round")
#' # If ROUND=0 then getMsg returns 'Runden Sie ihr Ergebnis auf eine ganze Zahl'
#' # If ROUND=1 then getMsg returns 'Runden Sie ihr Ergebnis auf eine Nachkommastelle'
#' # Otherwise getMsg retuns 'Runden Sie ihr Ergebnis auf `r ROUND` Nachkommastellen'
#' setMsg(ROUND='Runden Sie ihr Ergebnis auf `r ROUND` Nachkommastellen',
#'        '0'='Runden Sie ihr Ergebnis auf eine ganze Zahl',
#'        '1'='Runden Sie ihr Ergebnis auf eine Nachkommastelle')
#' getMsg(ROUND=0, .lang="de", .domain="round")
#' getMsg(ROUND=1, .lang="de", .domain="round")     
#' getMsg(ROUND=2, .lang="de", .domain="round")
setMsg <- function (..., .silent=TRUE) {
  isNamed <- function (nx) {
    if (is.null(nx)) return(FALSE)
    return(nchar(nx)>0)
  }
  #
  traverse_expr <- function(expr) {
    if (is.call(expr)) {
      args      <- as.list(expr)[-1]
      expr[-1]  <- lapply(args, traverse_expr)
      return(expr)
    } else if (is.name(expr)) {
      return(as.name(paste0('.', expr)))
    } else {
      return(expr)
    }
  }
  #
  #browser()
  args  <- list(...)
  stopifnot(length(args)>0)
  nargs <- names(args)
  if(any(!isNamed(nargs))) stop("All parameters must be named")
  key  <- names(args)[1]
  if (!is.null(msg[[msg$.domain]][[msg$.lang]][[key]])) warning(sprintf("Key '%s' already exists in language '%s'", key, msg$.lang))
  names(args)[1] <- ''
  args <- unlist(args)
  matches <- str_match_all(args, "`r\\s+(.*?)`")
  for (i in 1:length(args)) {
    nmi <- nrow(matches[[i]])
    if (nmi) {
      for (j in 1:nmi) {
        #browser()
        repl    <- paste0('`r ', deparse(traverse_expr(parse(text=matches[[i]][j,2])[[1]])), "`")
        args[i] <- gsub(matches[[i]][j,1], repl, args[i], fixed=TRUE)
      }
    }
  }
  .key <- paste0('.', key)
  # NOTE: args and matches are used for side effects with enviroment(fun) !!
  fun <- function(.param) {
    args <- environment(fun)$args
    env  <- parent.frame()$env
    env[[environment(fun)$.key]] <- .param
    param <- as.character(.param)
    txt  <- args[1]
    if (nchar(param) && (param %in% names(args))) txt <- args[param]
    knitr::knit(text=txt, envir=env, quiet=TRUE)
  }
  msg[[msg$.domain]][[msg$.lang]][[key]] <- fun
  if (!.silent) cat("  ", key, "\n")
  invisible(key)
}
