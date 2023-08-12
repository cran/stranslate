#' findText
#'
#' Finds strings matching `pattern` in `files`; for details for matching see [base::grepl()].
#'
#' @param files character: names of R file(s)
#' @param pattern character: pattern to match a string (default: `"\\w+{2,}"`)
#'
#' @return a text matrix with strings and file with line numbers
#' @importFrom utils getParseData
#' @export
#'
#' @examples
#' 1+1
findText <- function(files, pattern="\\w+{2,}") {
  ret <- matrix('', ncol=2, nrow=0)
  for (file in files) {
    cexpr <- getParseData(parse(file))
    cstr  <- cexpr[cexpr$token=="STR_CONST",]
    cstr  <- cstr[grepl(pattern, cstr$text),]
    ret   <- rbind(ret, cbind(cstr$text, paste(file, ":", cstr$line1)))
  }
  ret[order(ret[,1]),]
}
