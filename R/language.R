#' language
#'
#' Returns which loaded language will be used for finding a message depending on `.domain`.
#'
#' @param lang character: a language code, e.g. from ISO-639
#' @param available.languages character: names of languages (default: `NULL`). If `NULL` then list of loaded languages is used.
#' @param .domain character: domain namesd (default: `NULL`)
#'
#' @return character of languages use to find a message
#' @export
#'
#' @examples
#' print(options("stranslate.lang"))   # current default language
#' language("de_AT", c("de", "de_IT")) # request austrian german
#' #
#' loadMsg(system.file("messages", "messages.txt", package = "stranslate"))
#' language("de_AT")                   # request austrian german
#' language("tlh")                     # request klingon, not available -> "en"
language <- function(lang=getOption("stranslate.lang"), available.languages=NULL, .domain=NULL) {
  #browser()
  if (is.null(available.languages)) {
    if (is.null(.domain)) {
      for (d in names(msg)) available.languages <- c(available.languages, names(msg[[d]]))
    } else {
      available.languages <- names(msg[[.domain]])
    }
    available.languages <- unique(available.languages)
  }
  sal   <- available.languages[order(nchar(available.languages), decreasing = TRUE)]
  langs <- gsub("[^a-zA-Z0-9]+", ".", tolower(c(lang, sal)))
  langs <- sal[startsWith(langs[1], langs[-1])]
  if (!('en' %in% langs)) langs <- c(langs, 'en')
  langs
#  if (length(langs)) return(langs)
#  stop(sprintf("No language found for '%s' in %s", lang, paste0(available.languages, collapse=", ")))
}
