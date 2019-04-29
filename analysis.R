rm(list = ls())

library(tidyverse)
library(lme4)
library(yarrr)

ctr <- function(x) scale(x, scale = FALSE)

stim <- read_tsv("stimuli.tsv")
stim$item <- 1:nrow(stim)

cognate_properties <- read_csv("cognates_complete.csv")

vocab <- read_csv("vocabulary_tests_English_Spanish.csv")

prepare_data_frame <-  function(ixs){
  ixs <- left_join(ixs, stim, by = "item")
  ixs$word <- ifelse(ixs$cond == 1 | ixs$cond == 3, ixs$`Target word`, ixs$`Filler word`)
  ixs <- left_join(ixs, cognate_properties, by = c("word" = "English"))
  ixs$cognate <- factor(ifelse(ixs$cond == 1 | ixs$cond == 3, ifelse(ixs$`False Cognate`, "false_cognate", "cognate"), "control"), levels = c("cognate", "false_cognate", "control"))
  ixs$noise <- with(ixs, factor(ifelse(cond < 3, "no_noise", "noise"), levels = c("no_noise", "noise")))
  contrasts(ixs$cognate) <- contr.helmert
  colnames(contrasts(ixs$cognate)) <- c("cognate_vs_false_cognate", "true_false_cognate_vs_control")
  contrasts(ixs$noise) <- contr.helmert
  ixs <- left_join(ixs, vocab, by = "subj")
  return(ixs)
}

FFD <- read_csv("./IXS/COGFFD.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()

GD <- read_csv("./IXS/COGGD.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()

TVT <- read_csv("./IXS/COGTVT.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()


pirateplot(data = GD, R2 ~ cognate + noise + spanish)


ffd.lmm <- lmer(data = FFD, formula = R2 ~ cognate * noise + (noise|subj) + (1|item))


gd.lmm <- lmer(data = GD, formula = R2 ~ cognate  * ctr(english) * ctr(spanish) + noise + (1|subj) + (1|item))



tvt.lmm <- lmer(data = TVT, formula = R3 ~ cognate  * ctr(english) * ctr(spanish) + noise + (1|subj) + (1|item))


tvt.lmm <- lmer(data = TVT, formula = R2 ~ cognate * noise + (noise|subj) + (1|item))
