library(tidyverse)

delegaltak_raw <- readxl::read_excel(path = "data-raw/partdelegaltak_stata.xlsx")
szavazatok_raw <- readxl::read_excel(path = "data-raw/2018-04-08--egyéni és listás voksok szavazókör szerint_stata_fv.xlsx")
szavazatok <- szavazatok_raw %>%
  select ( megyeid, megye, telepid, telep, oevk, 
           szavazokor, ervenyes, ervenytelen, orsz_fidesz, 
           orsz_jobbik, 
           orsz_lmp, orsz_mszp, orsz_dk,
           orsz_egyutt, orsz_momentum, orsz_mkkp ) %>%
  mutate ( id = paste0(tolower(telep), "-", 
                       tolower(as.character(szavazokor))))

delegaltak <- delegaltak_raw %>%
  filter ( as.character(valasztas_datum) == "2018-04-08" )  %>%
  separate ( jelolocsoport, c("jelolo_code", "jelolo"), sep = " - ") %>%
  mutate ( id = tolower(ID), 
           megye = tolower (megye), 
           telepules = tolower (telepules), 
           oevk_szam = as.factor ( oevk_szam) ) %>%
  select ( id, megye, telepules, oevk_szam, jelolo_code, jelolo, biz_szint)
    
szavazokorok <- delegaltak %>% 
  select ( -jelolo_code ) %>%
  mutate ( count = 1) %>%
  mutate ( unique_number = 1:nrow(.)) %>%
  mutate ( jelolo = ifelse (nchar(jelolo)<2, NA, jelolo)) %>%
  mutate ( personal_id = paste0(as.character (id), "_", 
                                tolower(as.character(biz_szint)), "_" ,
                                tolower(as.character(jelolo )), "_", 
                                as.character(unique_number))) %>%
  mutate (jelolo = ifelse (is.na(jelolo), "NA", jelolo)) %>%
  mutate ( jelolo = ifelse ( jelolo == "FIDESZ-KDNP", "FIDESZ", jelolo)) %>%
  mutate ( jelolo = ifelse ( str_sub(jelolo,1,4) == "MSZP", "MSZP", jelolo)) %>%
  mutate ( jelolo = tolower(jelolo)) %>%
  mutate ( kistelepules = ifelse ( str_sub(id, -1,-1) == "-", 1, 0))

bizottsagok <- szavazokorok %>%
  spread (jelolo, count ) %>%
  group_by ( id ) %>%
  summarize_if ( is.numeric, sum, na.rm =TRUE) %>%
  select ( -unique_number ) %>%
  mutate ( total = rowSums(.[,c(2:ncol(.))])) %>%
  mutate ( ellenzek = jobbik + mszp  + lmp + dk+ momentum + mkkp + együtt ) %>%
  mutate ( is_fidesz = ifelse (fidesz >0,1,0)) %>%
  mutate ( is_ellenzek = ifelse (ellenzek >0,1,0)) %>%
  mutate ( nem_fidesz = total - fidesz) %>%
  mutate ( id = ifelse ( str_sub(id, -1,-1) == "-", paste0(id, "001"), id))

summary_bizottsag <- bizottsagok %>%
  select ( id, nem_fidesz, kistelepules) %>%
  mutate ( kistelepules = as.factor(kistelepules)) 

summary_bizottsag_2 <- bizottsagok %>%
  mutate ( ellenzek = jobbik + mszp  + lmp + dk+ momentum + mkkp + együtt ) %>%
  select ( id, fidesz, nem_fidesz, kistelepules, ellenzek ) %>%
  mutate ( is_fidesz = ifelse (fidesz >0,1,0)) %>%
  mutate ( is_ellenzek = ifelse (ellenzek >0,1,0)) %>%
  group_by (kistelepules ) %>%
  summarize ( fidesz_avg = mean(fidesz, na.rm=TRUE), 
              fidesz_median = median(fidesz, na.rm=TRUE), 
              fidesz_jelenlet = mean ( is_fidesz, na.rm=TRUE),
              ellenzek_avg = mean (ellenzek, na.rm=TRUE), 
              ellenzek_median = median  (ellenzek, na.rm=TRUE), 
              ellenzek_jelenlet = mean ( is_ellenzek, na.rm=TRUE))
  
szavazas <- bizottsagok %>%
  select ( id, kistelepules, total, fidesz, ellenzek, is_fidesz, is_ellenzek) %>%
  left_join (., szavazatok ) %>%
  mutate_at ( vars(starts_with("orsz_")), funs (./(ervenyes+ervenytelen) )) %>%
  mutate ( ervenytelen = ervenytelen / (ervenyes+ervenytelen ))

kistelepulesek <- szavazas %>%
  filter ( kistelepules == 1 ) %>%
  gather ( part, values, orsz_fidesz:orsz_mkkp) %>%
  mutate ( part = as.character(part)) %>%
  mutate ( part = gsub("orsz_", "", part)) %>%
  mutate ( values  = as.numeric (values )) %>%
  mutate ( part = forcats::fct_relevel(part, c("fidesz", "jobbik", "mszp", 
                                               "lmp", "dk", "momentum", 
                                               "mkkp", "egyutt"))) %>%
  filter ( ervenytelen > 0) %>%
  mutate ( is_ellenzek = ifelse ( is_ellenzek ==1, "van ellenzéki", "nincs ellenzéki"))


my_palette <- srvisual::sr_palette( ext = TRUE)


ggplot ( data = kistelepulesek, 
         aes ( x = ervenytelen, y = values, color = part, group = is_ellenzek)) +
  geom_point () +
  scale_color_manual ( values = c("#E88500", "#5C2320", "#DB001C", "#00843A", 
                                  "#00348A",  "#4E115A", "#282828", 
                                  "#FAE000")) +
  facet_wrap ( facets = "is_ellenzek")

kistelepulesek_summary <- kistelepulesek %>%
  select ( ervenytelen, part, values, is_ellenzek ) %>%
  mutate ( is_ellenzek = ifelse ( is_ellenzek ==1, "van ellenzéki", "nincs ellenzéki"))
ggplot ( data = kistelepulesek_summary, 
         aes ( x = ervenytelen, y = values, color = part)) +
  geom_smooth ( method = "lm") +
  facet_wrap ( facets = "part") +
  scale_color_manual ( values = c("#E88500", "#5C2320", "#DB001C", "#00843A", 
                                  "#00348A",  "#4E115A", "#282828", 
                                  "#FAE000"))


ggplot ( data = kistelepulesek, 
         aes ( x = ervenytelen, y = values, color = part)) +
  scale_y_continuous( labels = scales::percent ) +
  geom_smooth ( method = "lm") +
  facet_wrap ( facets = "part") +
  scale_color_manual ( values = c("#E88500", "#5C2320", "#DB001C", "#00843A", 
                                  "#00348A",  "#4E115A", "#282828", 
                                  "#FAE000")) +
  facet_grid ( part ~ is_ellenzek ) +
  theme ( legend.position = "none") +
  labs ( 
    x = "Érvénytelen szavazatok száma", 
    y = "Párt szavazataránya", 
    caption = "Forrás: github.com/antaldaniel", 
    title = "Mennyit számít az ellenzéki részvétel a kistelepüléseken?", 
    subtitle = "Összefüggés a szavazatszámláló bizottságok összetétele és a pártok szavazatai között")
