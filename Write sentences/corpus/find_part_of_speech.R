#rm(list=ls())

#setwd("E:/Bernhard/Documents/Experiments/ExperimentSHUF/English/mpos")

#pos <- read.csv("mobyposi.csv", as.is = TRUE)

#colnames(pos) <- c("word","pos")

#pos <- pos[!is.na(pos$pos),]

#save(pos, file = "mobipos.RData")

load("bnc.RData")

find_pos <- function(word, only_dominant_meaning = 0)
	{
		part_of_speech <- character(length(word))
		wordlist <- character(length(word))
		
		for(i in 1:length(word))
			{
			if(i%%100==0)
				{ cat("Element", i ,"of",length(word),"\n") }
			
			# First search the wordlist and re-use repeated tags
			if(sum(wordlist == tolower(word[i])) >= 1
				{	
					tags = part_of_speech[wordlist == tolower(word[i])]
					part_of_speech[i] <- tags[1] # select most frequent part of speech 
				}
			else
			{
				res <- bnc[bnc$word == tolower(word[i]),] #search for lower case version of word
				
				if(length(res[!is.na(res)]) == 0)
					{
						warning("'",word[i],"' not found. Is it spelled correctly? NA generated.")
						part_of_speech[i] <- NA
					}
				else
					{
						res <- res[!is.na(res$pos_simple),]
						part_of_speech[i] <- res[which.max(res$freq),]$pos # select most frequent part of speech 
					}

				if(only_dominant_meaning == 1 && nchar(part_of_speech[i]) > 1)
					{
						part_of_speech[i] <- substr(part_of_speech[i],1,1)
					}
				}
			}
		return(part_of_speech)
	}


