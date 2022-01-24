# c
require(data.table)
cat("\nBritish National Corpus (http://www.kilgarriff.co.uk/bnc-readme.html) associated functions: \n find_pos, find_bnc_freq.\n")




load("bnc.RData")


#Use aggregated bnc frequency for all PoS
#bnc_aggregate <- bnc %>%
#  group_by(word) %>%
#  summarize(total_freq = sum(freq), freq_per_million = sum(freq_per_million), wl = mean(wl))

#save(bnc, bnc_aggregate, file = "bnc.RData")

find_pos <- function(word, only_dominant_meaning = 0)
	{
		part_of_speech <- character(length(word))
		
		for(i in 1:length(word))
			{
			if(i%%100==0)
				{ cat("Element", i ,"of",length(word),"\n") }
			res <- bnc[bnc$word == tolower(word[i]),] #search for lower case version of word
			
			if(length(res[!is.na(res)]) == 0)
				{
					warning("'",word[i],"' not found. Is it spelled correctly? NA generated.")
					part_of_speech[i] <- NA
				}
			else
				{
					res <- res[!is.na(res$pos_simple),]
					part_of_speech[i] <- res[which.max(res$freq),]$pos_simple # select most frequent part of speech 
				}

			if(only_dominant_meaning == 1 && nchar(part_of_speech[i]) > 1) # no longer used!
				{
					part_of_speech[i] <- substr(part_of_speech[i],1,1)
				}
			}
		return(part_of_speech)
	}

find_bnc_freq <- function(word)
	{
		freq_per_million <- character(length(word))
		
		for(i in 1:length(word))
			{
			if(i%%100==0)
				{ cat("Element", i ,"of",length(word),"\n") }
			res <- bnc[bnc$word == tolower(word[i]),] #search for lower case version of word
			
			if(length(res[!is.na(res)]) == 0)
				{
					warning("'",word[i],"' not found. Is it spelled correctly? NA generated.")
					freq_per_million[i] <- NA
				}
			else
				{
					res <- res[!is.na(res$freq_per_million),]
					freq_per_million[i] <- res[which.max(res$freq),]$freq_per_million # select most frequent part of speech 
				}

			}
		return(freq_per_million)
	}


find_bnc_word_freq_new <- function(searched_word){
  bnc %>%
    filter(word == tolower(searched_word)) %>%
    summarize(token_freq = sum(freq_per_million))
}

find_bnc_word_freqs_new <- function(searched_words){
  unlist(sapply(X = searched_words, FUN = find_bnc_word_freq_new))
  #do(X = data.frame(searched_words), FUN = find_bnc_word_freqs_new(.))
}

find_bnc_word_freqs_aggregate <- function(searched_words){
  bnc_aggregate[bnc_aggregate$word %in% searched_words,]$freq_per_million
}
# then need to do a left join, e.g. left_join(FFD, bnc_aggregate[bnc_aggregate$word %in% FFD$word,], by = "word")