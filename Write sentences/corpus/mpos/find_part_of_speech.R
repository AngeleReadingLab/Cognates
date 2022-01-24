rm(list=ls())

setwd("E:/Bernhard/Documents/Experiments/ExperimentSHUF/English/mpos")

#pos <- read.csv("mobyposi.csv", as.is = TRUE)

#colnames(pos) <- c("word","pos")

#pos <- pos[!is.na(pos$pos),]

#save(pos, file = "mobipos.RData")

load("mobipos.RData")

find_pos <- function(word, only_dominant_meaning = 1)
	{
		part_of_speech <- character(length(word))
		
		for(i in 1:length(word))
			{
			res <- pos[pos$word == word[i],]$pos
			
			if(length(res[!is.na(res)]) == 0)
				{
					warning("'",word,"' not found. Is it spelled correctly? NA generated.")
					part_of_speech[i] <- NA
				}
			else
				{
					part_of_speech[i] <- res[!is.na(res)]
				}

			if(only_dominant_meaning == 1 && nchar(part_of_speech[i]) > 1)
				{
					part_of_speech[i] <- substr(part_of_speech[i],1,1)
				}
			}
		return(part_of_speech)
	}


