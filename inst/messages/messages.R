setLang('en') # adds to domain 'default'
setMsg('ROUND'='Round your result to `r ROUND` decimal places',
       '0'='Round your result to an integer',
       '1'='Round your result to one decimal place')
setMsg(PHOTO='`r user` added `r COUNT(PHOTO)` to `r STREAM(gender)` stream')
setMsg(COUNT='`r PHOTO` new photos',
       '1'='a new photo')
setMsg(STREAM='their',
       'male'='his',
       'female'='her')
#
setLang('de') # adds to domain 'default'
setMsg(ROUND='Runden Sie ihr Ergebnis auf `r ROUND` Nachkommastellen',
       '0'='Runden Sie ihr Ergebnis auf eine ganze Zahl',
       '1'='Runden Sie ihr Ergebnis auf eine Nachkommastelle')
setMsg(PHOTO='`r user` fügt `r COUNT(PHOTO)` `r STREAM(gender)` Stream zu')
setMsg(COUNT='`r PHOTO` neue Fotos',
       '1'= 'ein neues Foto')
setMsg(STREAM='seinem',
       'female'='ihrem')
