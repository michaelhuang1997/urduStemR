## A Urdu stemmer, modeled after the arabicStemR, but with substantial changes
## Michael Huang
## This version:

###########################################################
## A list of all chars in the Arabic unicode range
## I use this below to demonstrate which characters are being cleaned
## by the cleanChars() function.
triplet <- c(paste("0",c(60:69),sep=""),paste("06",c("A","B","C","D","E","F"),sep=""))
ArabicUnicodeChars <- as.vector(sapply(triplet,function(x){paste(x,c(0:9,c("A","B","C","D","E","F")),sep="")}))
x1 <- paste0("\\u",ArabicUnicodeChars)
ArabicUnicodeChars <- sapply(x1,function(x){parse(text = paste0("'", x, "'"))[[1]]})
rm(x1,triplet)

###########################################################
## Trim funtion
## This is used throughout to trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

###########################################################
## remove numbers

## Removes Latin character numbers
rmEngNum <- function(texts){
  texts <- gsub('[0-9]', ' ', texts)
  # remove extra spaces
  return(trim(gsub(" {2,}", " ", texts)))
}

## Removes Arabic character numbers
rmAraNum <- function(texts){
  texts <- gsub('[\u0660-\u0669]',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## Removes Urdu character numbers
rmUrdNum <- function(texts){
  texts <- gsub('[\u06f0-\u06f9]',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## bundles the three functions for removing numbers
rmNum <- function(texts){
  texts <- rmEngNum(texts)
  texts <- rmAraNum(texts)
  texts <- rmUrdNum(texts)
  return(texts)
}

###########################################################
## clean out junk characters

## removes punctuation
rmPunc <- function(texts){
  ## replace urdu specific puntuations
  texts <- gsub('\u060c|\u061b|\u061f|\u066c|\u066d|\u06d4|\u06dd|\u06de|\u06e9',' ',texts)
  ## replace other junk characters that sometimes show up
  texts <- gsub('[\u200C-\u200F]|&nbsp|~|\u2018|\u2022|\u2013|\u2026|\u201c|\u201d|\u2019|\ufd3e|\ufd3f', ' ', texts)
  ## replace general punctuations
  texts <- gsub("[[:punct:]]", " ", texts)
  ## remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## remove diacritics indicating Urdu vowels
rmDiac <- function(texts){
  ## replace diacritics
  texts <- gsub('[\u0610-\u061a]||\u0640|[\u064b-\u065f]|\u0670|[\u06d6-\u06dc]|[\u06df-\u06e4]|[\u06e7-\u06e8]|[\u06ea-\u06ed]', '', texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## remove \n \r \t \f \v
rmNewline <- function(texts){
  texts <- gsub('\n|\r|\t|\f|\v',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## bundle the functins for punctuation, diacritics, and newlines together.
clean <- function(texts){
  texts <- rmPunc(texts)
  texts <- rmDiac(texts)
  texts <- rmNewline(texts)
  return(texts)
}

#######################################################
## Standardize the alifs except alif maddah

fixAlifs <- function(texts){
  texts <- gsub('\u0623|\u0625|[\u0671-\u0673]|\u0675','\u0627', texts)
  return(texts)
}

#######################################################
## clean up the characters

## This function removes any characters in the text that are not in either the Latin unicode range
## or in the Arabic alphabet + "p".

##cleanChars <- function(texts){
  # http://jrgraphix.net/research/unicode_blocks.php
  ## ones I'm dropping
  ##texts <- gsub('[\u00A0-\u0600]|[\u0600-\u0621]|[\u063b-\u0640]|[\u064b-\u065f]|[\u066a-\u067d]|[\u067f-\u06ff]|[\u0700-\uFB4F]|[\uFB50-\uFDFF]|[\uFE00-\uFFFF]','',texts)
  ## I could sort through these ones too: http://jrgraphix.net/r/Unicode/FB50-FDFF, but I'm not right now
  ## clean up spaces
  ##return(trim(gsub(" {2,}"," ", texts)))
##}

## This function removes all latin characters using unicode ranges
cleanLatinChars <- function(texts){
  # http://jrgraphix.net/research/unicode_blocks.php
  ## ones I'm aiming to keep
  # texts <- gsub('[[:alpha:]]','',texts)
  # Romney fix
  texts <- gsub("\\p{Latin}", "", texts, perl = TRUE)
  ## I could sort through these ones too: http://jrgraphix.net/r/Unicode/FB50-FDFF, but I'm not right now
  ## clean up spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

#######################################################
## Remove stopwords

## Removes stopwords from a list that I've put together myself and from online sources as well
rmSW <- function(texts, defaultSWList=T, customSWList=NULL){
  # Split up the words...
  textsSplit = strsplit(texts," ")

  preps <- c('\u06a9\u06d2', #ke
             '\u06a9\u0627', #kaa
             '\u06a9\u06cc', #kee
             '\u0645\u06cc\u06ba', #main
             '\u0645\u06cc\u0646', #main with dot
             '\u0633\u06d2', #se
             '\u0628\u0627\u0648\u062c\u0648\u062f', #bawajood
             '\u0644\u0626\u06d2', #liye
             '\u0628\u062c\u0627\u0626\u06d2', #bajaaye
             '\u0633\u0627\u062a\u06be', #saath
             '\u0639\u0644\u0627\u0648\u06c1',  #alaawah
             '\u062f\u0631\u0645\u06cc\u0627\u06ba', #darmiyaan
             '\u062f\u0631\u0645\u06cc\u0627\u0646', #darmiyaan with dot
             '\u0646\u0632\u062f\u06cc\u06a9', #nazdeek
             '\u0628\u0631\u0639\u06a9\u0633', #bar-aks
             '\u067e\u06cc\u0686\u06be\u06d2', #peeche
             '\u0633\u0627\u0645\u0646\u06d2', #saamne
             '\u0630\u0631\u06cc\u0639\u06d2', #zariye
             '\u0633\u0628\u0628', #sabab
             '\u0628\u063a\u06cc\u0631', #baghair
             '\u0646\u0627\u0645', #naam
             '\u0641\u0648\u0642', #foq
             '\u0646\u06cc\u0686\u06d2', #neeche
             '\u0628\u0627\u06c1\u0631', #bahar
             '\u0644\u0641\u0638\u0648\u06ba\u0020', #lafzon
             '\u0628\u0631\u0627\u0628\u0631', #barabar
             '\u0628\u0631\u062a\u0631', #bartar
             '\u0627\u0648\u067e\u0631', #oopar
             '\u0627\u0646\u062f\u0631', #andar
             '\u0637\u0631\u0641', #taraf

  )

  # Demonstrative, subject and relative pronouns
  pronouns <- c('\u0645\u06cc\u06ba', #main
                '\u0622\u067e', #aap
                '\u062a\u0645', #tum
                '\u0648\u06c1', #vo
                '\u06c1\u0645', #ham
                '\u0627\u0633', #is/us
                '\u062a\u0648', #tu





  )








}




