library(MASS)
library(tidyverse)
library(lme4)
#library(lmerTest) # uncomment if you want p-values
library(yarrr)
library(brms)



# Read data files


source("./Write sentences/corpus/bnc_functions.R")


ctr <- function(x)
  scale(x, scale = FALSE)

stim <- read_tsv("stimuli.tsv")
stim$item <- 1:nrow(stim)

stim <-
  stim %>% mutate(tar_bnc = find_bnc_freq(`Target word`),
                  fill_bnc = find_bnc_freq(`Filler word`))

mean(as.numeric(stim$tar_bnc), na.rm = TRUE)

mean(as.numeric(stim$fill_bnc), na.rm = TRUE)

sd(as.numeric(stim$fill_bnc) - as.numeric(stim$tar_bnc), na.rm = TRUE)

cognate_properties <- read_csv("cognates_complete.csv")

vocab <- read_csv("vocabulary_tests_English_Spanish.csv")

prepare_data_frame <-  function(ixs) {
  ixs_new <- ixs %>% left_join(stim, by = "item") %>%
    mutate(word = case_when(
      cond == 1 | cond == 3 ~ `Target word`,
      cond == 2 |
        cond == 4 ~ `Filler word`
    )) %>% # make  a column that gives the word in the target position no matter which condition we're in
    left_join(cognate_properties, by = c("word" = "English")) %>%
    left_join(vocab, by = "subj") %>%
    left_join(bnc_aggregate[bnc_aggregate$word %in% .$word, ],
              by = "word",
              suffix = c("", ".BNC")) %>%
    mutate(noise = factor(
      case_when(cond > 2 ~ "noise",
                cond <= 2 ~ "no_noise"),
      levels = c("no_noise", "noise")
    )) %>%
    mutate(
      condition = factor(
        case_when(
          !`False Cognate` & (cond == 1 | cond == 3) ~ "cognate",
          `False Cognate` & (cond == 1 | cond == 3) ~ "false_cognate",!`False Cognate` &
            (cond == 2 | cond == 4) ~ "cognate_control",
          `False Cognate` &
            (cond == 2 | cond == 4) ~ "false_cognate_control"
        ),
        levels = c(
          "cognate",
          "false_cognate",
          "cognate_control",
          "false_cognate_control"
        )
      ),
      exp_vs_control = factor(case_when((cond == 1 |
                                           cond == 3) ~ "exp",
                                        (cond == 2 |
                                           cond == 4) ~ "control"
      ),
      levels = c("exp", "control")),
      word_pair = factor(
        ifelse(`False Cognate`, "false_and_control", "cognate_and_control"),
        levels = c("cognate_and_control", "false_and_control")
      )
    )
  
  my_contrasts <-
    cbind(
      cognate_vs_false_cognate = c(-1, 1, 0, 0),
      cognate_vs_control = c(-1, 0, 1, 0),
      false_cognate_vs_fc_control = c(0, -1, 0, 1)
    )
  contrasts(ixs_new$condition) <- zapsmall(t(ginv(my_contrasts)))
  colnames(contrasts(ixs_new$condition)) <- colnames(my_contrasts)
  contrasts(ixs_new$noise) <- contr.sum
  contrasts(ixs_new$exp_vs_control) <- contr.sum
  contrasts(ixs_new$word_pair) <- contr.sum
  
  
  return(ixs_new)
}
#c(C, F, CC, FC)
# Contrast 1: CC-C = c(-1,0,1,0)
# Contrast 2: FC-F = c(0,-1,0,1)
# Contrast 3: (CC-C) - (FC-F) = CC - C - FC + F = -C, F, CC, -FC c(-.5,.5,.5,-.5)
# Can't do this because they're redundant (rank deficient)
# So instead do a contrasts comparing the target words: New Contrast 2: C-FC =  c(0,0,1,-1)

FFD <-
  read_csv("./IXS/COGFFD.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()

GD <-
  read_csv("./IXS/COGGD.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()

TVT <-
  read_csv("./IXS/COGTVT.IXS") %>% filter(!is.na(R1)) %>% prepare_data_frame()

# analysis for FFD

FFD.lmm <-
  lmer(
    data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
    formula = R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 /
                                                                                            freq_per_million) + (1 | subj) + (1 | item)
  )

FFD.lm <-
  lm(
    data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
    formula = R2 ~ condition * noise * ctr(english) * ctr(spanish) * ctr(freq_per_million)
  )

FFD_means <-
  FFD %>% group_by(condition, noise) %>% summarize(M = mean(R2, na.rm = TRUE))

pirateplot(R2 ~ condition + noise,
           data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
           theme = 3)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(x = freq_per_million, y = R2, colour = condition)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish) * ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(
         x = ctr(spanish) * ctr(english) * freq_per_million,
         y = R2,
         colour = condition
       )) + geom_smooth() + facet_grid(noise ~ .)

# analysis for GD

GD.lmm <-
  lmer(
    data = GD %>% filter(!(subj %in% c(16, 34, 40))),
    formula = R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 /
                                                                                            freq_per_million) + (1 | subj) + (1 | item)
  )

pirateplot(R2 ~ condition + noise,
           data = GD %>% filter(!(subj %in% c(16, 34, 40))),
           theme = 3)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(x = freq_per_million, y = R2, colour = condition)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish) * ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(
         x = ctr(spanish) * ctr(english) * freq_per_million,
         y = R2,
         colour = condition
       )) + geom_smooth() + facet_grid(noise ~ .)

# analysis for TVT

TVT.lmm <-
  lmer(
    data = TVT %>% filter(!(subj %in% c(16, 34, 40))),
    formula = R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 /
                                                                                            freq_per_million) + (1 | subj) + (1 | item)
  )

pirateplot(R2 ~ condition + noise,
           data = TVT %>% filter(!(subj %in% c(16, 34, 40))),
           theme = 3)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(x = freq_per_million, y = R2, colour = condition)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), aes(
  x = ctr(spanish) * ctr(english),
  y = R2,
  colour = condition
)) + geom_smooth() + facet_grid(noise ~ .)
ggplot(data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
       aes(
         x = ctr(spanish) * ctr(english) * freq_per_million,
         y = R2,
         colour = condition
       )) + geom_smooth() #+ facet_grid(noise ~ .)

# BRMS models

ncores = parallel::detectCores()
# Mean RTs in each condition

# Check the priors that brm sets by default
 default_prior_eye_movements <- get_prior(data = FFD %>% filter(!(subj %in% c(16, 34, 40))), formula = bf(
  R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) | item),
  beta ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) | item)),
  family = exgaussian())
# The default priors for fixed effect coefficients are flat. Use weakly informative (mean 0) but slightly more realistic priors for the fixed effects 
prior_eye_movements <-
  c(
    set_prior("normal(0,100)", class = "b", coef = "ctr1Dfreq_per_million"),
    set_prior("normal(0,1000)", class = "b", coef = "ctrenglish"),
    # slightly more variance here since this coefficient is likely to be quite large due to the way the variable is scaled (difference between no proficiency and perfect proficiency is 1)
    set_prior("normal(0,1000)", class = "b", coef = "ctrspanish"),
    # slightly more variance here since this coefficient is likely to be quite large due to the way the variable is scaled (difference between no proficiency and perfect proficiency is 1)
    set_prior("normal(0,100)", class = "b", coef = "exp_vs_control1"),
    set_prior("normal(0,100)", class = "b", coef = "exp_vs_control1:noise1"),
    set_prior("normal(0,100)", class = "b", coef = "noise1"),
    set_prior("normal(0,100)", class = "b", coef = "word_pair1"),
    set_prior("normal(0,100)", class = "b", coef = "word_pair1:exp_vs_control1"),
    set_prior("normal(0,100)", class = "b", coef = "word_pair1:exp_vs_control1:noise1")
  )

# need to set inits to "0" since sampling from [-2, 2] (which "random" does") doesn't work
# better inits may be possible

blmm_ffd_dist <-
  brm(
    data = FFD %>% filter(!(subj %in% c(16, 34, 40))),
    formula = bf(
      R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item),  beta ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item)
    ),
    warmup = 2000,
    iter = 10000,
    chains = 4,
    prior = prior_eye_movements,
    family = exgaussian(),
    inits = "0",
    control = list(adapt_delta = 0.9),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(4)
  )

save(blmm_ffd_dist, file = "blmm_ffd_dist3.RData")

blmm_gd_dist <-
  brm(
    data = GD %>% filter(!(subj %in% c(16, 34, 40))),
    formula = bf(
      R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item),  beta ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item)
    ),
    warmup = 2000,
    iter = 10000,
    chains = 4,
    prior = prior_eye_movements,
    family = exgaussian(),
    inits = "0",
    control = list(adapt_delta = 0.9),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(4)
  )

save(blmm_gd_dist, file = "blmm_gd_dist3.RData")


blmm_tvt_dist <-
  brm(
    data = TVT %>% filter(!(subj %in% c(16, 34, 40))),
    formula = bf(
      R2 ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item),  beta ~ word_pair * exp_vs_control * noise + ctr(english) + ctr(spanish) + ctr(1 / freq_per_million) + (word_pair * exp_vs_control * noise + ctr(1 / freq_per_million) | subj) + (exp_vs_control * noise + ctr(english) + ctr(spanish) | item)
    ),
    warmup = 2000,
    iter = 10000,
    chains = 4,
    prior = prior_eye_movements,
    family = exgaussian(),
    inits = "0",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(4)
  )

save(blmm_tvt_dist, file = "blmm_tvt_dist3.RData")

# analyse the fits


