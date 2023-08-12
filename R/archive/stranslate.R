msg <- new.env()

loadMsg <- function(files, .lang='en', silent=TRUE) {
  #browser()
  key <- ' '
  for (file in files) {
    if (!silent) cat(" Read file:", file, "\n")
    suppressWarnings({
      lines <- readLines(files)
    })
    lines <- lines[nchar(lines)>0]          # delete empty lines
    lines <- lines[!startsWith(lines, '#')] # delete comment lines
    lind  <- startsWith(lines, "<")
    lang  <- trimws(c(.lang, substring(lines[lind], 2)))
    for (langi in lang) if(is.null(msg[[langi]])) msg[[langi]] <- list()
    tok1 <- sub("\\s.*", "", lines)
    msgi <- NULL
    lang <- .lang
    i    <- 1
    while(i<=length(lines)) {
      if (tok1[i]=="<") {
        lang <- trimws(substring(lines[i], 2))
        if (!silent) cat(" Language:", lang, "\n")
      } else {
        stopifnot(substr(lines[i], 1, 1) %in% c(letters, LETTERS))
        # get key
        key   <- tok1[i]
        if (!grepl("^[[:alnum:]_.]+$", key)) {
          stop(sprintf("Only letters, digits, dot and underscores allowed in key '%s'.", key))
        }
        msgi  <- character(0)
        query <- ''
        msgi[query] <- trimws(substring(lines[i], 1+nchar(key)))
        j      <- i+1
        while (j<=length(lines)) {
          if (tok1[j]=="") msgi[query] <- paste0(msgi[query], "\n", trimws(substr(lines[j])))
          if (startsWith(tok1[j], '?')) {
            query <- trimws(substring(tok1[j], 2))
            msgi[query] <- trimws(substring(lines[j], 2+nchar(query)))
          }
          if (!(substr(lines[j], 1, 1) %in% c("", "?"))) break
          j <- j+1
        }
        if (!is.null(msg[[lang]][[key]])) warning(sprintf("Key '%s' already exists in language '%s'", key, lang))
        msg[[lang]][[key]] <- paste0(sprintf("switch('{$%s}', ", key),
                                     paste0(ifelse(names(msgi)=='', '', sprintf("'%s'=", names(msgi))), sprintf("'%s'", msgi), collapse=", "),
                                     ')')
        if (!silent) cat("  ", key, "\n")
        i <- j-1
      }
      i <- i+1
    }
  }
  if (!silent) {
    keys <- NULL
    for (l in names(msg)) {
      kl <- names(msg[[l]])
      kb <- unlist(msg[[l]])
      kb <- unique(unlist(regmatches(kb, gregexpr("\\{([^\\{\\}]+)\\}", kb)))) # extract templates
      kb <- strsplit(gsub("\\{|\\}|\\s", "", kb), "$", fixed=TRUE)
      keys <- c(keys, kl, sapply(kb, '[', 1))
    }
    keys <- unique(setdiff(keys, ""))
    for (l in names(msg)) {
      lkey <- setdiff(keys, names(msg[[l]]))
      if (length(lkey)) warning(sprintf("Missing for language '%s' the key(s): %s", l, paste0(lkey, collapse=", ")))
    }
  }
}

getMsg <- function (..., .lang=Sys.getenv('LANG')) {
  fallback <- function(lang, available.languages) {
    available.languages <- available.languages[order(nchar(available.languages), decreasing = TRUE)]
    lang  <- gsub("[^a-zA-Z0-9]+", ".", tolower(c(lang, available.languages)))
    available.languages[startsWith(lang[1], lang[-1])]
  }
  #
  buildArgs <- function(args) {
    if (length(args)==0) return(args)
    wargs <- list()
    nargs <- names(args)
    if (is.null(nargs)) nargs <- rep('', lentgh(args))
    for (i in 1:length(args)) {
      wargs[[as.character(i)]] <- args[[i]]
      if (nargs[i]!='') wargs[[nargs[i]]] <- args[[i]]
    }
    wargs
  }
  #
  as_character <- function(object) {
    if (length(object)>1) stop("'key' must have length 1, maybe '\"key\"' works")
    if (inherits(object, 'call'))       object <- deparse(object)
    if (!inherits(object, 'character')) object <- toString(object)
    object
  }
  template <- function(key, primary, args, lang) {
    #browser()
    for (l in lang) {
      txt     <- msg[[l]][[key]]
      if (!is.null(txt)) {
        matches <- unique(unlist(regmatches(txt, gregexpr("\\{([^\\{\\}]+)\\}", txt)))) # extract templates
        extract <- strsplit(gsub("\\{|\\}|\\s", "", matches), "$", fixed=TRUE)
        for (i in 1:length(extract)) {
          replace <- as_character(args[[extract[[i]][2]]])
          if (nchar(extract[[i]][1])) replace <- template(extract[[i]][1], replace, args, lang)
          #browser()
          txt <- gsub(matches[i], replace, txt, fixed=TRUE)
        }
        return(eval(parse(text=txt)))
      }
    }
    keys <- NULL
    for (l in names(msg)) keys <- c(keys, names(msg[[l]]))
    keys <- unique(keys)
    dkey <- adist(keys, key)
    keys <- keys[order(dkey)]
    if (length(keys)>5) keys <- keys[1:5]
    stop(sprintf("Key '%s' not found in '%s', maybe you meant:\n  %s", key, paste0(lang, collapse=", "),
                 paste0("  ", keys, collapse="\n")))
  }
  #
  args  <- match.call()[-1]
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  browser()
  key     <- ifelse(nargs[1]=='', as_character(args[[1]]), nargs[1])
  primary <- ifelse(nargs[1]=='', "", as_character(args[[1]]))
  args    <- buildArgs(args)
  lang    <- fallback(.lang, names(msg))
  #browser()
  return(template(key, primary, args, lang=lang))
}


loadMsg('messages.txt', silent=FALSE)
getMsg(RUNDEN=3, .lang="de_de")

getMsg(SHARED-PHOTOS, user="Anne", gender="female", photo=3)
