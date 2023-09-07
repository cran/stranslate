#' findText
#'
#' Extract all texts, `getMsg`, or `setMsg` calls in the R files given. 
#' Note that the `'` and `"` are part of the string, thus `'ROUND'` has a length of 7!
#'
#' @param files character: names of R files to analyse
#' @param pattern character: pattern to match a string (default: `"\\w+{2,}"`)
#'
#' @return a data frame with the columns
#' * `file` with file name
#' * `line` the line number
#' * `type` if a string constant (`STR_CONST`), a `getMsg` (`GETMSG`), or a `setMsg` (`SETMSG`) was found
#' * `text` the text of the string constant or the key used in `getMsg` or `setMsg`
#' @importFrom utils getParseData str
#' @export
#'
#' @examples
#' getText(system.file("messages", "messages.R", package="stranslate"))
getText <- function(files, pattern="\\w+{2,}") {
  dr <- data.frame(file=character(0), line=integer(0), type=character(0), text=character(0))
  for (file in files) {
    if (file.exists(file)) {
      df <- getParseData(parse(file), includeText = TRUE)
      if (is.data.frame(df)) {
        indgm <- which(startsWith(df$text, "getMsg("))
        for (i in seq_along(indgm)) {
          dfi      <- which(df$parent==df$id[indgm[i]])
          indgm[i] <- dfi[3] 
        }
        indsm <- which(startsWith(df$text, "setMsg("))
        for (i in seq_along(indsm)) {
          dfi      <- which(df$parent==df$id[indsm[i]])
          indsm[i] <- dfi[3] 
        }
        indsc <- which((df$token=="STR_CONST") & grepl(pattern, df$text))
        df <- df[c(indgm, indsm, indsc),]
        dr <- rbind(dr, data.frame(file=rep(normalizePath(file), nrow(df)), 
                                   line=df$line1, 
                                   type=c(rep("GETMSG", length(indgm)), rep("SETMSG", length(indsm)), rep("STR_CONST", length(indsc))), 
                                   text=df$text))
      }
    }
  }
  ludf <- length(unique(dr$file))
  if (ludf<1) return(NULL)
  if (ludf==1) {
    dr$file <- basename(dr$file)
  } else {
    drf <- strsplit(dr$file, '/', fixed=TRUE)
    i <- 1
    repeat {
      v <- sapply(drf, "[", i)
      if (length(unique(v))>1) break
      i <- i+1
    }
    dr$file <- sapply(drf, i=i, FUN=function(v, i) { paste0(v[i:length(v)], collapse = "/") })
  }
  dr[order(dr$file, dr$line),]
}