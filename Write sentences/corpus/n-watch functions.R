cat("\nN-Watch associated functions: find_celex, find_celex_bf (MLBF), ascend_descend_profile, find_adp,\n get_bigrams, get_bigram_freq, mean_bigram_freq, mean_log_bigram_freq and get_overlap.\n")

load("D:/Bernhard/Documents/Experiments/ExperimentSHUF/English/celex_mlbf.RData") # from N-Watch!
load("D:/Bernhard/Documents/Experiments/ExperimentSHUF/English/BF.RData")

find_celex <- function(x) {
              celex <- character(length(x))

              for(i in 1:length(x))
                {
                  res <- cx[cx$word == tolower(x[i]),]
				  if(length(res[!is.na(res),]$celex_wrt) > 0)
					{
						#celex[i] <- cx[cx$word == x[i],]$celex_wrt
						celex[i] <- res$celex_wrt
					}
				  else
					{
				    warning("'",x[i],"' not found. Is it spelled correctly? NA generated.")
					celex[i] <- NA
					}
                }
                as.numeric(celex)
                }
				

                
#find_celex_bf <- function(x) { cx[cx$word == x,]$mlbf_tk}

find_celex_bf <- function(x) {
              celex <- character(length(x))

              for(i in 1:length(x))
                {
                  celex[i] <- cx[cx$word == x[i],]$mlbf_tk
                }
                as.numeric(celex)
                }

ascend_descend_profile <- function(x)
		{

			ascenders <- c("t","d","f","h","k","l","b")
			descenders <- c("q","y","p","g","j")

			profiles <- NA

			for(i in 1:length(x))
			{
			split_str <- strsplit(as.character(x),"", perl=TRUE)[[i]]
			profile <- as.numeric(split_str %in% ascenders)
			profile <- profile + 2 * as.numeric(split_str %in% descenders)
			profile <- paste(as.character(profile), collapse = "")
			profiles[i] <- profile
			if(i%%100 == 0) 	{ cat(sprintf("%i out of %i done!\n",i,length(x)))}
			}
		return(profiles)
		}

find_adp <- function(x,stop_at = 99) {
		              x_length = nchar(x)
				  x <- substr(x,1,stop_at)
				  alt <- cx[substr(cx$ad_profile,1,stop_at)==ascend_descend_profile(x) & nchar(cx$ad_profile) == x_length,]
				  alt
				}



get_bigrams <- function(x)
		{
			x_length = nchar(x)
			bigrams <- data.frame(position = 1:(x_length-1), bigram = NA)

			for(i in 1:(x_length-1))
			 {
				bigrams$bigram[i] <- substr(x,i,i+1)
			 }
			bigrams
		}

get_bigram_freq <- function(x,token = TRUE)

		{
			if(token == TRUE) { t_string <- "TK"}
			if(token == FALSE) { t_string <- "TP"}

			x_length <- nchar(x)

			bigrams <- get_bigrams(x)

			for(i in 1:nrow(bigrams))
				{
					bigram_column_name <- sprintf("L%iP%i_%s",x_length,i,t_string)
					bigram_row <- BF[BF$Bigram == bigrams$bigram[i],]
					bigrams$freq[i] <- eval(parse(text = sprintf("bigram_row$%s",bigram_column_name)))
				}
				bigrams
		}

mean_bigram_freq <- function(x, token = TRUE)
		{
			mean_bf_tk <- numeric(0)
			for(i in 1:length(x))
 			 {

			mean_bf_tk[i] <- mean(get_bigram_freq(x[i], token)$freq)
			if(i%%100 == 0) 	{ cat(sprintf("%i out of %i done!\n",i,length(x)))}
			 }
			mean_bf_tk
		}

mean_log_bigram_freq <- function(x, token = TRUE)
		{
			mean_bf_tk <- numeric(0)
			for(i in 1:length(x))
 			 {
          if(!is.na(x[i]))
          {
            bfreqs <- get_bigram_freq(x[i])
      			bfreqs$log <- log10(bfreqs$freq)
      			bfreqs$log[bfreqs$log == -Inf] <- 0

      			mean_bf_tk[i] <- mean(bfreqs$log)
          } else { mean_bf_tk[i] <- NA }
          
          if(i%%100 == 0) 	{ cat(sprintf("%i out of %i done!\n",i,length(x)))}
			 }
			mean_bf_tk
		}

get_overlap <- function(x,y)
    {
    if(nchar(x) == nchar(y))
	   {
	     overlaps <- numeric(nchar(x))
       for(i in 1:nchar(x))
	       {
	         overlaps[i] <- as.numeric(substr(x,i,i) == substr(y,i,i))
         }
     sum(overlaps)

	   } else { warning("Get_overlap: Words must be of equal length! NA generated.")
              NA }
     }

