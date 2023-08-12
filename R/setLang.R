#' setLang
#'
#' Sets the language and the domain.
#'
#' @param .lang character: language (default: `"en"`)
#' @param .domain character: domain (default: `getOption("stranslate.domain")`)
#'
#' @return invisibly the language set
#' @export
#'
#' @examples
#' setLang('tlh')  # use klingon now ;)
setLang <- function(.lang='en', .domain=getOption("stranslate.domain")) {
  if (!isName(.domain)) stop(sprintf("Only letters, digits, dot and underscores allowed in domain '%s'.", .domain))
  if (is.null(msg[[.domain]])) msg[[.domain]] <- list()
  if (is.null(msg[[.domain]][[.lang]])) msg[[.domain]][[.lang]] <- list()
  msg$.lang <- .lang
  msg$.domain <- .domain
  invisible(msg$.lang)
}
