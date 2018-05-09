library(tidyverse)

tstat_1 <- readxl::read_excel("data-raw/Telepulesadatok_tidy.xls", 
                            sheet = 1 )
names (tstat_1)[2]= "telepuleskod"
names ( tstat_1)[1] = "telep"

tstat_2 <- readxl::read_excel("data-raw/kiegeszito_telepules_adat.xls", 
                              sheet = 1 )
names (tstat_2)[2]= "telepuleskod"
adofizetok <- readxl::read_excel("data-raw/adofizetok_szama.xls", 
                                 sheet = 1 )
names (adofizetok)[2]= "telepuleskod"

nepesseg <- readxl::read_excel(
  "data-raw/allando_nepesseg_18_felett_2014_16_tidy.xls", 
  sheet = 1 )
names (nepesseg)[2]= "telepuleskod"
names (nepesseg)[1] = "telep"

tstat <- left_join (tstat_1, tstat_2, by = c("telep", "telepuleskod", "nepesseg")) %>%
  left_join( ., adofizetok, by = c("telep", "telepuleskod")) %>%
  left_join(., nepesseg, by = c("telep", "telepuleskod")) %>%
  mutate ( adofizeto_rate = adofizetok / nepesseg_18p )

View (tstat)
