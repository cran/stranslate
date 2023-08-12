## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(stranslate)
options(stranslate.lang='en')

## -----------------------------------------------------------------------------
library("stranslate")
file <- system.file("messages", "messages.txt", package="stranslate")
loadMsg(file) # load the translation(s)

## -----------------------------------------------------------------------------
listMsg()     # show all loaded keys and domains

## -----------------------------------------------------------------------------
getMsg(ROUND=3)
getMsg(ROUND=3, .lang="de")
getMsg(ROUND=1)
getMsg(ROUND=1, .lang="de")

## -----------------------------------------------------------------------------
loadMsg(system.file("messages", "Rdewiki.txt", package="stranslate")) # load the translation(s)
getMsg("Rdewiki", .lang="en")
getMsg(Rdewiki, .lang="de")

## -----------------------------------------------------------------------------
getMsg(PHOTO=5, user="Anne", gender="female")
getMsg(PHOTO=5, user="Anne", gender="female", .lang="de")
getMsg(PHOTO=0, user="Bert", gender="male")
getMsg(PHOTO=1, user="Bert", gender="male", .lang="de")

## -----------------------------------------------------------------------------
# use German spoken in South Tirol (Italy)
# but available onby standard german and german spoken in Austria
language('de_IT', c('de', 'de_AT'))

## -----------------------------------------------------------------------------
loadMsg(file, .domain="mydomain") # load the translation(s)
listMsg()

## ---- eval=FALSE--------------------------------------------------------------
#  options(stranslate.domain='mydomain')

## ---- eval=FALSE--------------------------------------------------------------
#  # switch to german
#  < de
#  # start key with its default message, note that `r ROUND` is replaced by its value
#  ROUND  Runden Sie ihr Ergebnis auf `r ROUND` Nachkommastellen
#  # if ROUND==0 then use this message
#  ?0     Runden Sie ihr Ergebnis auf eine ganze Zahl
#  # if ROUND==1 then use this message
#  ?1     Runden Sie ihr Ergebnis auf eine Nachkommastelle
#  # otherwise use the default message

## ---- eval=FALSE--------------------------------------------------------------
#  ROUND  `r user` runde dein Ergebnis auf `r ROUND` Nachkommastellen

## ---- eval=FALSE--------------------------------------------------------------
#  PHOTO   `r user` added `r COUNT(PHOTO)` to `r STREAM(gender)` stream
#  COUNT   `r PHOTO` new photos
#  ?1      a new photo
#  STREAM  their
#  ?male   his
#  ?female her

## -----------------------------------------------------------------------------
setLang('en', "R")   # choose language and another domain
setMsg('ROUND'='Round your result to `r ROUND` decimal places',
       '0'='Round your result to an integer',
       '1'='Round your result to one decimal place')
setMsg(PHOTO='`r user` added `r COUNT(PHOTO)` to `r STREAM(gender)` stream')
setMsg(COUNT='`r PHOTO` new photos', '1'='a new photo')
setMsg(STREAM='their', 'male'='his', 'female'='her')
#
setLang('de', 'R')
setMsg(ROUND='Runden Sie ihr Ergebnis auf `r ROUND` Nachkommastellen',
       '0'='Runden Sie ihr Ergebnis auf eine ganze Zahl',
       '1'='Runden Sie ihr Ergebnis auf eine Nachkommastelle')
setMsg(PHOTO='`r user` fügt `r COUNT(PHOTO)` `r STREAM(gender)` Stream zu')
setMsg(COUNT='`r PHOTO` neue Fotos', '1'= 'ein neues Foto')
setMsg(STREAM='seinem', 'female'='ihrem')
#
getMsg(ROUND=3, .domain="R")

## ---- echo=FALSE, comment=''--------------------------------------------------
file <- system.file("messages", "messages.txt", package="stranslate")
cat(readLines(file), sep="\n")

## ---- echo=FALSE, comment=''--------------------------------------------------
file <- system.file("messages", "Rdewiki.txt", package="stranslate")
cat(readLines(file), sep="\n")

## ---- echo=FALSE, comment=''--------------------------------------------------
file <- system.file("messages", "stranslate.txt", package="stranslate")
cat(readLines(file), sep="\n")

## ---- echo=FALSE, comment=''--------------------------------------------------
file <- system.file("messages", "messages.R", package="stranslate")
cat(readLines(file), sep="\n")

