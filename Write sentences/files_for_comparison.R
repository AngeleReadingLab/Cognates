rm(list = ls())

library(tidyverse)
library(stringi)
library(writexl)

load("./corpus/bnc.RData")

load("./corpus/coca.RData")

# make aggregated version of the coca

#Use aggregated bnc frequency for all PoS
coca_aggregate <- c %>%
  group_by(word) %>%
  summarize(coca_total_freq = sum(freq), coca_freq_per_million = sum(freq_per_mill))

#save(c, coca_aggregate, file = "./corpus/coca.RData")

rm(c) #just use coca_aggregate

cognates <- read_delim("cognate_table.tsv", delim = "\t", skip = 1, 
                       col_names = c("English", "Part_of_Speech", 
                                     "Is_Cognate_EN_PT", "Is_Cognate_EN_SP", "Is_Cognate_PT_SP",
                                     "Is_False_Cognate_EN_PT", "Is_False_Cognate_EN_SP","Is_False_Cognate_PT_SP", 
                                     "Similar_Word_PT","Similar_Word_SP", 
                                     "Translation_PT", "Translation_SP",
                                     "Comments")) %>%
  distinct(English, .keep_all = TRUE) # remove any duplicate rows based on the English word
  

cognates$Similar_No_Accents_PT <-stri_trans_general(cognates$Similar_Word_PT, "Latin-ASCII")
cognates$Similar_No_Accents_SP <-stri_trans_general(cognates$Similar_Word_SP, "Latin-ASCII")


# we will use PT as the first word (the comparison word according to Davis' SOLAR approach to similarity)
# and EN as the second word (the input)
PT_EN <- cognates %>%
  filter(!(Is_Cognate_EN_PT == "no" & Is_False_Cognate_EN_PT == "no")) %>% 
  select(Similar_No_Accents_PT, English)


write_tsv(PT_EN, "PT_EN for comparison.txt", col_names = FALSE)

# we will use SP as the first word (the comparison word according to Davis' SOLAR approach to similarity)
# and EN as the second word (the input)

# Match Calculator has some issues when there is 0 overlap, so we remove all words where we don't expect any

SP_EN <- cognates %>%
  filter(!(Is_Cognate_EN_SP == "no" & Is_False_Cognate_EN_SP == "no")) %>% 
  select(Similar_No_Accents_SP, English)


write_tsv(SP_EN, "SP_EN for comparison.txt", col_names = FALSE)



# excluding non-cognates (and false friends) seems not to be necessary for PT/SP

# we will use PT as the first word (the comparison word according to Davis' SOLAR approach to similarity)
# and SP as the second word (the input)

PT_SP <- cognates %>%
  filter(!(Is_Cognate_PT_SP == "no" & Is_False_Cognate_PT_SP == "no")) %>% 
  select(Similar_No_Accents_PT, Similar_No_Accents_SP)

write_tsv(PT_SP, "PT_SP for comparison.txt", col_names = FALSE)

PT_EN_compared <- read_tsv(file = "PT_EN compared.txt", skip = 14, col_names = c("Portuguese","English","Absolute", "Vowel-Centric_L_R", "Vowel-Centric_R_L", "Ends-first", "SOLAR","Binary Open Bigram", "Overlap Open Bigram", "SERIOL", "test"))

SP_EN_compared <- read_tsv(file = "SP_EN compared.txt", skip = 14, col_names = c("Spanish", "English", "Absolute", "Vowel-Centric_L_R", "Vowel-Centric_R_L", "Ends-first", "SOLAR","Binary Open Bigram", "Overlap Open Bigram", "SERIOL", "test"))

PT_SP_compared <- read_tsv(file = "PT_SP compared.txt", skip = 14, col_names = c("Portuguese", "Spanish", "Absolute", "Vowel-Centric_L_R", "Vowel-Centric_R_L", "Ends-first", "SOLAR","Binary Open Bigram", "Overlap Open Bigram", "SERIOL", "test")) %>%
  distinct(Portuguese, Spanish, .keep_all = TRUE)

cognates_PT_EN <- left_join(x = cognates, y = PT_EN_compared, by = "English")

cognates$SOLAR_PT_EN <- cognates_PT_EN$SOLAR

cognates_SP_EN <- left_join(x = cognates, y = SP_EN_compared, by = "English")

cognates$SOLAR_SP_EN <- cognates_SP_EN$SOLAR

cognates_PT_SP <- left_join(x = cognates, y = PT_SP_compared, by = c("Similar_No_Accents_PT" = "Portuguese", "Similar_No_Accents_SP" = "Spanish"))

cognates$SOLAR_PT_SP <- cognates_PT_SP$SOLAR

cognates_bnc <- left_join(x = cognates, y = bnc_aggregate, by = c("English" = "word"))

cognates_coca <- left_join(x = cognates_bnc, y = coca_aggregate, by = c("English" = "word"))

port_corpus <- read_tsv(file = "./corpus/wl_cb_full_1gram_sketchengine.txt", skip = 7, col_names = c("word", "freq"))
port_corpus$freq_per_million <- port_corpus$freq/sum(port_corpus$freq)*10^6

cognates_port <-left_join(x = cognates_coca, y = port_corpus, by = c("Similar_Word_PT" = "word"))
colnames(cognates_port)[colnames(cognates_port) == "freq_per_million.y"] <- "PT_freq_per_million"

lexesp <- read_tsv("./corpus/bpal_nwfreq.txt", col_names = c("word", "SP_freq_per_million"))
cognates_complete <- left_join(x = cognates_port, y = lexesp, by = c("Similar_Word_SP" = "word"))

old20 <-read_tsv("old20.txt", col_names = c("word", "OLD20"))
cognates_orthographic <- left_join(x = cognates_complete, y = old20, by = c("English" = "word"))


write_csv(cognates_orthographic, "cognates_orthographic.csv")

writexl::write_xlsx(x = cognates_orthographic, path = "cognates_orthographic.xlsx")
