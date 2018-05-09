library(tidyverse)
library(caret)
library(rpart)
library(tabplot)

szavstat <- readRDS("data/szavstat.rds") %>%
  select ( -megyeid, -telepid, -ervenytelen, -ervenyes, 
           - starts_with("orsz_"), -id, -telepuleskod, 
           -odavandorlas, -elvandorlas, -idosek, 
           -lakas, -jovedelem, -adofizetok, -jovedelem_fo, 
           -ujszulottek, -lakas_fo, -kabeltv, -kabeltv_rate) 

kistelepulesek <- szavstat %>% filter ( n == 1) %>%
  filter ( complete.cases(.))

training.samples <- kistelepulesek$reszvetel_est %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- kistelepulesek[training.samples, ]
test.data <- kistelepulesek[-training.samples, ]

paste (names(kistelepulesek), collapse = "', '")

variable_reszvetel <- c('megye', 'oevk', 
                    'fidesz_rate', 'jobbik_rate', 'mszp_rate', 
                    'lmp_rate', 'dk_rate', 'momentum_rate',
                    'mkkp_rate', 'egyutt_rate',
                    'ervenytelen_rate', 'megye_ido_km', 
                    'budapest_ido_km', 'megyeszekhely_km', 
                    'budapest_km', 'megyeszekhely_min',
                    'budapest_min', 'ujszulott_rate',
                    'idosek_rate', 'nepesseg', 
                    'vandorlas_rate', 'altisk_rate',
                    'nepesseg_2014_18p', 'nepesseg_2015_18p',
                    'nepesseg_18p', 'adofizeto_rate')

as.formula(paste0 ("reszvetel_est ~",  paste(variable_reszvetel, collapse = "+")))
set.seed(2018)
model_reszv <- train(
  as.formula(paste0 ("reszvetel_est ~",  paste(variable_reszvetel, collapse = "+"))), data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model error vs different values of
# cp (complexity parameter)
plot(model_reszv)
# Print the best tuning parameter cp that
# minimize the model RMSE
model_reszv$bestTune

par(xpd = NA) # Avoid clipping the text in some device
plot(model_reszv$finalModel)
text(model_reszv$finalModel, digits = 3)
model_reszv$finalModel

kistelepulesek_lm <- kistelepulesek %>%
  mutate ( adofizeto_0038 = ifelse (adofizeto_rate < 0.3850082, 1, 0)) %>%
  mutate ( adofizeto_5599 = ifelse (adofizeto_rate > 0.5539019, 1, 0)) %>%
  mutate ( altisk_rate_0016 = ifelse (altisk_rate < 16.45, 1, 0)) %>%
  mutate ( altisk_rate_2699 = ifelse ( altisk_rate > 26.25, 1, 0)) %>%
  mutate ( iskolazottsag = 1-altisk_rate)


reszvetel_lm <-lm ( reszvetel_est ~ adofizeto_rate + iskolazottsag, 
                    data = kistelepulesek_lm)
summary(reszvetel_lm)
plot ( reszvetel_lm)

kistelepulesek <- kistelepulesek %>%
  mutate ( reszvetel_pred = predict(reszvetel_lm))



