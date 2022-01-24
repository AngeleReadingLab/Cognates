rm(list=ls())
setwd("D:/Bernhard/Documents/Experiments/ExperimentSHUF/English")

source("n-watch functions.R")
source("bnc_functions.R")

s <- read.csv2("boycow_short.csv", as.is = TRUE)

s$pos <- find_pos(s$word_no_pct)

s[s$word_no_pct == "carhop",]$pos <- "noun"
s[s$word_no_pct == "eggnog",]$pos <- "noun"
s[s$word_no_pct == "potpie",]$pos <- "noun"
s[s$word_no_pct == "fishhook",]$pos <- "noun"

s$freq_bnc <- find_bnc_freq(s$word_no_pct)

s$freq_bnc <- as.numeric(s$freq_bnc)

s$freq_celex <- find_celex(s$word_no_pct)

save(file="boycow_short.RData",s)
writeMat("boycow_short.mat",sentence = s)
