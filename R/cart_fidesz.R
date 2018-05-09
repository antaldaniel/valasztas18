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
  filter ( complete.cases(.)) %>%
  mutate ( baranya = as.factor(ifelse (megye == "BARANYA", 1,0))) %>%
  mutate ( vas = as.factor(ifelse (megye == "VAS", 1,0))) %>%
  mutate ( borsod = as.factor(ifelse ( grepl("BORSOD", megye), 1, 0))) %>%
  mutate ( elidosodott = as.factor (ifelse ( idosek_rate > 0.28528, 1, 0)))

training.samples <- kistelepulesek$reszvetel_est %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- kistelepulesek[training.samples, ]
test.data <- kistelepulesek[-training.samples, ]

paste (names(kistelepulesek), collapse = "', '")

variable_fidesz <- c('baranya', 'vas', 'borsod', 'oevk', 
                    'fidesz_rate', 
                    'ervenytelen_rate', 'megye_ido_km', 
                    'budapest_ido_km', 'megyeszekhely_km', 
                    'budapest_km', 'megyeszekhely_min',
                    'budapest_min', 'ujszulott_rate',
                    'idosek_rate', 'nepesseg', 
                    'vandorlas_rate', 'altisk_rate',
                    'nepesseg_18p', 'adofizeto_rate')

set.seed(2018)
model_fidesz <- train(
  as.formula(paste0 ("fidesz_rate ~",  
                     paste(variable_fidesz, collapse = "+"))), 
  data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model error vs different values of
# cp (complexity parameter)
plot(model_fidesz)
# Print the best tuning parameter cp that
# minimize the model RMSE
model_fidesz$bestTune

par(xpd = NA) # Avoid clipping the text in some device
plot(model_fidesz$finalModel, branch = 0.8, compress = T)
text(model_fidesz$finalModel, digits = 3, fancy = F)
print ( model_fidesz$finalModel )
rpart.plot::prp(model_fidesz$finalModel,
                yes.text = "igaz", 
                no.text  = 'hamis') 

rattle::fancyRpartPlot(model_fidesz$finalModel, palettes = Oranges)
?rpart.plot::prp
?rpart.prp
library(rattle)	
rpart ( model_fidesz$finalModel)

kistelepulesek_lm <- kistelepulesek %>%
  mutate ( adofizeto_0038 = ifelse (adofizeto_rate < 0.3850082, 1, 0)) %>%
  mutate ( adofizeto_5599 = ifelse (adofizeto_rate > 0.5539019, 1, 0)) %>%
  mutate ( altisk_rate_0016 = ifelse (altisk_rate < 16.45, 1, 0)) %>%
  mutate ( altisk_rate_2699 = ifelse ( altisk_rate > 26.25, 1, 0)) %>%
  mutate ( iskolazottsag = 1-altisk_rate) 


fidesz_lm <-lm ( fidesz_rate ~ nepesseg_18p + budapest_km + log(idosek_rate) +
                      altisk_rate + vas + baranya + borsod, 
                    data = kistelepulesek)
summary(fidesz_lm)
plot ( fidesz_lm)

fidesz_pred <- predict(fidesz_lm)

ggplot( data = kistelepulesek, aes ( x = budapest_km, y = fidesz_rate, group = nepesseg_18p )) +
  geom_smooth ( data = kistelepulesek, method = "lm", formula = y~ x  )


