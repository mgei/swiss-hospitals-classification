# prepares the app data


# read the data
fallzahlen <- read_rds("../data/fallzahlen_clean.RDS") %>%
  mutate_if(is.character, funs(gsub("\u00AD", "-", ., perl = F))) %>% 
  filter(Spitäler != "Alle Fälle")

spitalstatistik  <- read_excel("../data/kzp16_daten.xlsx", sheet = "KZ2016_KZP16") %>% 
  filter(!is.na(KT)) %>% 
  # soft-hyphon is trouble
  mutate_if(is.character, funs(gsub("\u00AD", "-", ., perl = F)))

# what we need from spitalstatistik
# spistat_read %>% select(Inst, Akt, EtMedL, EtSonst, EtSubv) %>% distinct() -> spistat

spitalstatistik <- spitalstatistik %>%
  select(Inst, starts_with("Et"), Akt) %>%
  distinct() %>%
  rowwise() %>%
  mutate(Ertrag = sum(EtMedL, EtSonst, EtSubv)) %>%
  ungroup() %>%
  select(Inst, Akt, Ertrag)

fallzahlen %>%
  left_join(spitalstatistik, by = c("Spitäler" = "Inst")) %>% 
  select(-i, -Jahr) -> appdata

# reduce size: remove all with less than 1 case
appdata %>% filter(Fallzahlen > 1) -> appdata

appdata %>% write_rds("../data/appdata.RDS")

