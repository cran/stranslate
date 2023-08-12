msg <- new.env()

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("stranslate.lang")))   options(stranslate.lang=Sys.getenv('LANG'))
  if (is.null(getOption("stranslate.domain"))) options(stranslate.domain='default')
  loadMsg(system.file("messages", "stranslate.txt", package = "stranslate"), .domain="stranslate", .silent=TRUE)
}
