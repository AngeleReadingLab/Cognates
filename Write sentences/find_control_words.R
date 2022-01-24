rm(list=ls())

library(tidyverse)
#library(writexl)

load("corpus/coca.RData")

coca_aggregate$wl <- nchar(coca_aggregate$word)

blp <- read_tsv("blp-stimuli.txt")

blp <- blp[order(blp$bnc.frequency.million),]

cognates <- read_csv("cognates_orthographic.csv")



find_matches <- function(wl_to_match, freq_to_match, number_of_matches = 20){
  matches <- blp %>%
    filter(nletters == wl_to_match) %>%
    mutate(fdiff = abs(bnc.frequency.million - freq_to_match)) %>%
    arrange(fdiff)
  return(matches[1:number_of_matches,]$spelling)
  
}

find_matches_bnc <- function(wl_to_match, freq_to_match, number_of_matches = 20){
  matches <- bnc_aggregate %>%
    filter(wl == wl_to_match) %>%
    mutate(fdiff = abs(freq_per_million - freq_to_match)) %>%
    arrange(fdiff)
  return(matches[1:number_of_matches,]$word)
  
}

find_matches_coca <- function(wl_to_match, freq_to_match, number_of_matches = 20){
  matches <- coca_aggregate %>%
    filter(wl == wl_to_match) %>%
    mutate(fdiff = abs(coca_freq_per_million - freq_to_match)) %>%
    arrange(fdiff)
  return(matches[1:number_of_matches,]$word)
}


find_matches_word <- function(word, number_of_matches = 20){
  word_data = filter(cognates, English == word)
  wl_to_match = word_data$wl
  freq_to_match = word_data$freq_per_million.x
  cat(wl_to_match, "\nFreq: ", freq_to_match, "\n")
  find_matches(wl_to_match, freq_to_match, number_of_matches)
}

find_matches_word_bnc <- function(word, number_of_matches = 20){
  word_data = filter(cognates, English == word)
  wl_to_match = word_data$wl
  freq_to_match = word_data$freq_per_million.x
  cat(wl_to_match, "\nFreq: ", freq_to_match, "\n")
  find_matches_bnc(wl_to_match, freq_to_match, number_of_matches)
}

find_matches_word_coca <- function(word, number_of_matches = 20){
  word_data = filter(coca_aggregate, word == word)
  wl_to_match = word_data$wl
  freq_to_match = word_data$coca_freq_per_million
  cat(wl_to_match, "\nFreq: ", freq_to_match, "\n")
  find_matches_coca(wl_to_match, freq_to_match, number_of_matches)
}


candidates <- character(nrow(cognates))
for(i in 1:nrow(cognates)){
  candidates[i] <- with(cognates, paste(find_matches_bnc(wl[i], freq_per_million.x[i]), collapse = " "))
}

cognates$candidates <- candidates

writexl::write_xlsx(cognates, "cognates_candidates_bnc.xlsx")

