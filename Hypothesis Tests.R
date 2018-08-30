library(nlme)
library(multcomp)
library(psych)
library(sjstats)
library(readr)

library(dplyr)

library(tibble)
library(broom)
library(magrittr)

films <- read_csv("data/cm_films_n2574_reallyclean.csv") %>% filter(year <= 1979)

films$Era <- ordered(films$TimeGroup)
contrasts(films$Era) <- contr.poly(3) 

films$Place <- ordered(films$IsRus)
contrasts(films$Place) <- contr.treatment(2) 

films$spoken_languages_number[is.na(films$spoken_languages_number)] <- 0
films$Sound <- ordered(as.logical(films$spoken_languages_number))
contrasts(films$Sound) <- contr.treatment(2) 

rus <- films %>% filter(IsRus == TRUE)
world <- films %>% filter(IsRus == FALSE)


hts_ru <- list()



mkan_anova <- function(model) {
  ret <- list()
  ret$model <- model
  ret$anova <- car::Anova(model, type=3)
  ret$assum <- check_assumptions(model)
  ret$result <- anova_stats(ret$anova)
  ret$post_mcp <- glht(ret$model, linfct=mcp(Era = "Seq"))
  ret$post_summary <- summary(ret$post_mcp, test = adjusted(type = "free"))
  
  return (ret)
}



era_group_sizes <-table(rus$Era)

# make_umbrella_contrMat <- function(sizes, mul = 1, add_intercept=TRUE) {
#   ret <- contrMat(sizes, "UmbrellaWilliams") * mul
#   if(add_intercept) {
#     ret <- cbind(0, ret)
#   }
#   return(ret)
# }

hts_ru$era_msl <- mkan_anova(lm(shot_msl ~ Era + Sound, data=rus ))
hts_ru$era_asl <- mkan_anova(lm(shot_asl ~ Era + Sound, data=rus ))
hts_ru$era_sd <- mkan_anova(lm(shot_sd ~ Era + Sound, data=rus ))
hts_ru$era_cv <- mkan_anova(lm(shot_cv ~ Era + Sound, data=rus ))
