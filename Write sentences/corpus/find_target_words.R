rm(list=ls())

setwd("D:/Bernhard/Documents/Experiments/ExperimentSHUF/English")

source("n-watch functions.R")

cx$wordlength <- nchar(cx$word)

bnc <- read.table("written.al", fill = TRUE, as.is = TRUE)

colnames(bnc) <- c("freq","word","pos","number_of_files")

bnc$wl <- nchar(as.character(bnc$word))
bnc$freq <- as.numeric(bnc$freq)
bnc$pos_short <- substr(bnc$pos,1,2)
bnc$pos_one <- substr(bnc$pos,1,2)

bnc$pos_simple <- NA

bnc[substr(bnc$pos_short,1,1) == 'v',]$pos_simple <- "verb"
bnc[substr(bnc$pos_short,1,1) == 'n',]$pos_simple <- "noun"
bnc[substr(bnc$pos_short,1,2) == 'aj',]$pos_simple <- "adjective"
bnc[substr(bnc$pos_short,1,2) == 'av',]$pos_simple <- "adverb"
bnc[substr(bnc$pos_short,1,2) == 'at',]$pos_simple <- "article"
bnc[substr(bnc$pos_short,1,2) == 'cj',]$pos_simple <- "conjunction"
bnc[substr(bnc$pos_short,1,2) == 'cr',]$pos_simple <- "numeral"
bnc[substr(bnc$pos_short,1,1) == 'd',]$pos_simple <- "determiner"
bnc[substr(bnc$pos_short,1,1) == 'e',]$pos_simple <- "existential_there"
bnc[substr(bnc$pos_short,1,1) == 'i',]$pos_simple <- "interjection"
bnc[substr(bnc$pos_short,1,2) == 'or',]$pos_simple <- "numeral"
bnc[substr(bnc$pos_short,1,2) == 'pn',]$pos_simple <- "pronoun"
bnc[substr(bnc$pos_short,1,2) == 'po',]$pos_simple <- "possesive"
bnc[substr(bnc$pos_short,1,2) == 'pr',]$pos_simple <- "preposition"
bnc[substr(bnc$pos_short,1,2) == 'to',]$pos_simple <- "to"
bnc[substr(bnc$pos_short,1,2) == 'un',]$pos_simple <- "unclassified"
bnc[substr(bnc$pos_short,1,2) == 'xx',]$pos_simple <- "not"
bnc[substr(bnc$pos_short,1,2) == 'zz',]$pos_simple <- "letter"

bnc$freq_per_million <- bnc$freq/bnc$freq[1]*1000000

save(file = "bnc.RData",bnc)

# verbs5l <- with(bnc,bnc[pos_simple == "verb" & wl == 5,])

# verbs5l_hi <- with(bnc,bnc[pos_simple == "verb" & wl == 5 & freq_per_million > 50,])
# verbs5l_lo <- with(bnc,bnc[pos_simple == "verb" & wl == 5 & freq_per_million <= 10,])



##make a list of high frequency 5-letter words

# hifreq <- cx[cx$wordlength == 5 & cx$celex_wrt > 50,]$word

# hving <- hifreq[substr(hifreq,nchar(hifreq) - 2, nchar(hifreq)) == "ing"]
# his <- hifreq[substr(hifreq,nchar(hifreq), nchar(hifreq)) == "s"]

# lofreq <- cx[cx$wordlength == 5 & cx$celex_wrt < 10,]$word

# lving <- lofreq[substr(lofreq,nchar(lofreq) - 2, nchar(lofreq)) == "ing"]
# los <- lofreq[substr(lofreq,nchar(lofreq), nchar(lofreq)) == "s"]