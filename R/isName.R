#' isName
#'
#' Checks if a `txt` is a valid name:
#'
#' * A name must start with a letter and can be a combination of letters, digits, period(.) and underscore(_).
#' * Reserved words cannot be used as name (TRUE, FALSE, NULL, if...)
#'
#' @param txt character: name(s) to check
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' isName("?plot")
#' isName(".default")
isName<- function(txt) {
  reserved_words <- c("if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE",
                      "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
  grepl("^[a-zA-Z][a-zA-Z0-9._]*$", txt) && !(txt %in% reserved_words)
}
