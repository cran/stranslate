#' setMsg
#'
#' @param ... aaa
#' @param .silent logical: should during the loading process some output shown and tests after loading run
#'
#' @return aaa
#' @export
#'
#' @examples
#' 1+1
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
  #args    <- gsub("'", "\'", lapply(args, toString))
  names(args)[1] <- ''
  args <- unlist(args)
  matches <- str_match_all(args, "`r\\s+(.*?)`")
  for (i in 1:length(args)) {
    nmi <- nrow(matches[[i]])
    if (nmi) {
      for (j in 1:nmi) {
        browser()
        repl    <- paste0('`r ', deparse(traverse_expr(parse(text=matches[[i]][j,2])[[1]])), "`")
        args[i] <- gsub(matches[[i]][j,1], repl, args[i], fixed=TRUE)
      }
    }
  }
  .key <- paste0('.', key)
  fun <- function(.param) {
    args <- environment(fun)$args
    env  <- parent.frame()$env
    env[[environment(fun)$.key]] <- .param
    txt  <- if (as.character(.param) %in% names(args)) args[as.character(.param)] else args[1]
    knitr::knit(text=txt, envir=env)
  }
  msg[[msg$.domain]][[msg$.lang]][[key]] <- fun
#  msg[[msg$.domain]][[msg$.lang]][[key]] <- args
#  msg[[msg$.domain]][[msg$.lang]][[key]] <- paste0("c(",
#                                                   paste0(sprintf("%s'%s'", name, args), collapse=","),
#                                                   ')')
#  msg[[msg$.domain]][[msg$.lang]][[key]] <- paste0(sprintf("switch('{$%s}',", nargs[1]),
#                                                   paste0(sprintf("%s'%s'", name, args), collapse=","),
#                                                   ')')
  if (!.silent) cat("  ", key, "\n")
}
