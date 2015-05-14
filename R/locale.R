detectLocale <- function () {
  locale <- unlist(strsplit(Sys.getlocale("LC_CTYPE"), ".", fixed=TRUE))[[1]]
  return(locale)
}

systemLocale <- function() {
  message("Detecting system locale ... ", appendLF = FALSE)

  # get system locale
  info <- systemInfo()
  raw <- as.character(info[[20]])
  parts <- strsplit(unlist(strsplit(raw, ";",  fixed=TRUE)), "-", fixed=TRUE)
  
  if (length(parts[[1]]) >= 2) {
    # normalize locale to something like en_US
    locale <- paste(tolower(parts[[1]][1]), toupper(parts[[1]][2]), sep="_")
  } else {
    locale <- paste(tolower(parts[[1]][1]), toupper(parts[[1]][1]), sep="_")
  }
  message(locale)
  return(locale)
}

systemInfo <- function () {
  raw <- system("systeminfo /FO csv", intern=TRUE, wait=TRUE)
  info <- read.csv(textConnection(raw))
  return(info)
}