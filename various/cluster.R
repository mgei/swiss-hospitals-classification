# some code trying to make a cluster

library(tidyverse)
library(readxl)
library(cluster)
library(ggrepel)
library(ggiraph)

# read the data
fallzahlen <- read_rds("../data/fallzahlen_clean.RDS") 

spitalstatistik  <- read_excel("../data/kzp16_daten.xlsx", sheet = "KZ2016_KZP16") %>% 
  filter(!is.na(KT)) %>% 
  # soft-hyphon is trouble
  mutate_if(is.character, funs(gsub("\u00AD", "-", ., perl = F)))

# shrink to the columns that we want to filter on later
hospitals <- spitalstatistik %>% 
  select(institution = Inst, starts_with("Et"), Akt) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(revenue = sum(EtMedL, EtSonst, EtSubv)) %>% 
  select(institution, revenue, Akt) %>% 
  ungroup()

# only Akutspitäler
fallzahlen_akut <- fallzahlen %>% 
  left_join(hospitals, by = c("Spitäler" = "institution")) %>% 
  filter(revenue > 250*1000000,
         str_detect(Akt, "A")) %>% 
  group_by(DRG) %>% 
  filter(sum(Fallzahlen) > 0) %>% 
  ungroup()

# simplify
fallzahlen_akut_simply <- fallzahlen_akut %>% 
  select(Spitäler, Bez, Fallzahlen) %>% 
  group_by(Spitäler, Bez) %>% 
  summarise(Fallzahlen = sum(Fallzahlen)) %>% 
  ungroup() 

# Anzahl Spitäler
fallzahlen_akut_simply %>% select(Spitäler) %>% distinct() %>% nrow()
# Anzahl DRGs
fallzahlen_akut_simply %>% select(Bez) %>% distinct() %>% nrow()

# prepare data for cluster // PROPORTIONAL
clusterdata <- fallzahlen_akut_simply %>%
  group_by(Spitäler) %>%
  mutate(Fallzahlen_prop = Fallzahlen/sum(Fallzahlen)) %>% 
  ungroup() %>%
  select(Spitäler, Bez, Fallzahlen_prop) %>%
  distinct() %>%
  spread(Bez, Fallzahlen_prop) %>%
  replace(., is.na(.), as.double(0))


# We need the rownames for the matrix conversion, ignore the warning
rownames(clusterdata) <- clusterdata %>% select(Spitäler) %>% pull()

clusterdata_m <- clusterdata %>% select(-Spitäler) %>% as.matrix()

## assume number k of clusters
k <- 3

## compute distance matrix
distMtrx <- clusterdata_m %>% dist(diag=F) 

# ## build dendogram to visualize hierarchichal clustering
# hClusts <- distMtrx %>% hclust()
# hClusts %>% ggdendrogram(rotate = TRUE, theme_dendro = FALSE) +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank())
# 
## perform k-medoids clustering with pam
k_med_clust <- distMtrx %>% pam(k = k)

clustering <- k_med_clust$clustering %>% 
  as.factor() %>% 
  as.data.frame() %>% 
  rownames_to_column("Spitäler") %>% 
  as_tibble() %>% 
  rename(cluster = 2)

## perform multidimensional scaling to visualize distances in 2 dimensions 
mltDmScl <- distMtrx %>%
  cmdscale() %>%
  as.data.frame() %>%
  rownames_to_column("Spitäler") %>% 
  as_tibble() %>% 
  left_join(clustering, by = "Spitäler")

## plot clustering results and dissimilarities of RegionGruppe
mltDmScl %>% 
  left_join(hospitals, by = c("Spitäler" = "institution")) %>% 
  ggplot(aes(V1, V2, color = cluster, size = revenue)) + 
  geom_point_interactive(alpha = 0.6, aes(tooltip = Spitäler %>% str_remove_all("\'"))) +
  # coord_fixed(ratio = 1, xlim = c(-3,1.5)) + 
  labs(x = "Component 1", y = "Component 2", color="Cluster",
       title = "k-medoids Cluster", subtitle = "Datenjahr 2016, k = 4",
       caption = "Quelle: BAG Fallzahlen & Spitalstatistik") +
  #geom_text_repel(aes(label = Spitäler), size = 3) +
  #geom_text_repel(aes(label = if_else(Ertrag > 500000000, Spitäler, "")), size = 3) +
  scale_size(guide="none") +
  theme_bw() -> myplot

ggiraph(code = print(myplot))
