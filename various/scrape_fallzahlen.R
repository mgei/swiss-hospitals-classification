# downloads data from http://www.drg-fallzahlsuche.admin.ch

library(tidyverse)
library(xml2)
library(rvest)

scrape_fallzahlen <- function(link = "http://www.drg-fallzahlsuche.admin.ch/de/systems/6/codes/", 
                              from = 1, to = 3661, 
                              getstring = "?level=Adrg", 
                              save_every_n = 100, 
                              childsafety = T) {
  
  if (childsafety) { stop("Turn off childsafety after you had a look at the code.")}
  
  fallzahlen <- tibble()
  
  for (i in 1:to) {
    url <- str_c(link, i, getstring)
    
    data <- read_html(url)
    
    info <- data %>% html_nodes("h2") %>% html_text() %>% last() %>% str_split("\n", simplify = T)
    DRG <- info[2]
    Jahr <- info[3]
    Bez <- info[4]
    
    table <- data %>% html_table(header = T) %>% 
      data.frame() %>% 
      as_tibble() %>% 
      mutate(DRG = DRG, Jahr = Jahr, Bez = Bez, i = i) %>% 
      mutate(Fallzahlen = as.character(Fallzahlen))
    
    fallzahlen <- fallzahlen %>% bind_rows(table) 
    
    if (!i %% save_every_n | i == to) {
      # save it every n loops and at the end obviously
      fallzahlen %>% write_rds(str_c("../data/data", i, ".RDS"))
    }
    print(str_c("Saved at ", i, sep = ""))
  }
  
  # clean the data
  fallzahlen_clean <- fallzahlen %>% 
    mutate(Fallzahlen = str_replace(Fallzahlen, "< 5", "1") %>% 
             str_remove_all("\'")) %>% 
    mutate(Fallzahlen = as.integer(Fallzahlen))
  
  fallzahlen_clean %>% write_rds("../data/fallzahlen_clean.RDS")
}



