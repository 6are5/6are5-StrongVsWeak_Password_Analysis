# Install entity https://github.com/trinker/entity
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/entity")
pkgs <- c('entity', 'logging', 'stringi')
sapply(pkgs, library, character.only = TRUE)

# A helper for handling ugly list-of-list manipulation
unlisted <- function(x) sapply(x, function(a) ifelse(is.null(unlist(a)), NA, unlist(a)))

# Basic Python `logging` style logger
standardLogger <- function(log_out, level = 20) {
  basicConfig(level = level)
  addHandler(writeToFile, logger = 'status', file = log_out)
}

# Simple tagger. Runs all annotators on `x`, produces character matrix of columns per annotation.
omniTagger <- function(
  x, 
  tagger = c("person_annotator", "location_annotator", "date_annotator", "money_annotator", "percent_annotator")
)
{
  ret <- list()
  for (i in seq_along(tagger)) {
    ret[[i]] <- unlisted(entity::named_entity(x, tagger[i]))
  }
  stri_list2matrix(ret)
}

# Performs NER in batches, pushing features to disk
tags_to_disk <- function(
  file_in = '~/Downloads/breachcompilation.txt', 
  file_out = '~/develop_foss/pw-analysis/data/ner.features',
  batch_size = 10**5,
  tag_system = omniTagger,
  log_level = 10,
  log_location = '~/develop_foss/pw-analysis/data/ner.log',
  logger = standardLogger
)
{
  # TODO: Error handling
  logger(log_location, log_level)
  logdebug('Initializing...')
  counter <- 0
  while(TRUE) {
    loginfo(paste('Reading from buffer', file_in))
    buffer <- readr::read_lines(file_in, skip = counter, n_max = batch_size)
    if(length(buffer) == 0) {
      loginfo('Tagging completed. Exiting...')
      return(0)
    }
    sz <- length(buffer); counter <- counter + sz;
    loginfo(paste('Buffer read. Size:', sz, 'Counter:', counter))
    buffer <- tag_system(buffer)
    loginfo('Buffer tagged.')
    logdebug('Head of tags\n', head(buffer))
    readr::write_lines(buffer, path = file_out, append = TRUE)
    loginfo(paste('Buffer written to', file_out))
  }
}

tags_to_disk()
