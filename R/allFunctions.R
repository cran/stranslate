#' allFunctions
#'
#' Returns the names of all functions currently available.
#'
#' @return character vector of functions names
#' @references https://stackoverflow.com/questions/4267744/is-there-a-way-to-get-a-vector-with-the-name-of-all-functions-that-one-could-use
#' @export
#'
#' @examples
#' allFunctions()
allFunctions <- function() {
  getFunctions <- function(pkgs) {
    nm <- ls(pkgs, all.names = TRUE)
    nm[unlist(lapply(nm, function(n) exists(n, where = pkgs,
                                            mode = "function",
                                            inherits = FALSE)))]
    if(isTRUE(all.equal(length(nm), 0))) return(character(0))
    nm2 <- strsplit(nm, '.', fixed = TRUE)
    nm2 <- setdiff(sapply(nm2, function(v) { paste0(v[-length(v)], collapse='.') }), '')
    c(nm, nm2)
  }
  #
  pkgs <- search()
  pkgs <- pkgs[grep("package:", pkgs)]
  z <- lapply(pkgs, getFunctions)
  unique(unlist(z))
}
