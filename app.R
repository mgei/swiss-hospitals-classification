# shiny app

library(shiny)
library(tidyverse)
library(cluster)
# library(ggrepel)
library(ggiraph)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidytext)

appdata <- readRDS("data/appdata.RDS")

# appdata <- read_feather("appdata.feather")

rot <- 0.1 # proportion of words rotated 90 degrees [set to default]
nwords <- 80 # max number of words to be plotted
cex.scale <- c(2,0.5) # relative scale of word sizes, i.e. size of biggest and smallest word [set to default]

mycolors <- brewer.pal(4, "Set1")

# 1. UI ----
ui <- fluidPage(
  
  titlePanel("Clusteranalyse Schweizer Spitäler"),
  
  # 1.1. User input and Cluser ----
  fluidRow(
    column(3, 
           checkboxGroupInput("Akt", "Spitaltyp", 
                              choiceNames = c("Akut", "Psych", "Reha", "Birth"),
                              choiceValues = c("A", "P", "R", "B"),
                              selected = c("A")),
           sliderInput("k", "Clusteranzahl k", min = 1, max = 4, value = 3),
           # numericInput("Ertrag_min", "Ertrag mind. (Mio.)", 200, min = 0, max = 1000, step = 5),
           sliderInput("Ertrag_min", "Ertrag mind. (Mio.)", min = 0, max = 1000, value = 200, step = 5)
    ),
    column(9, ggiraphOutput("myplot"))
  ),
  
  # 1.2. Wordclouds ----
  fluidRow(
    
    column(3, plotOutput("wcplot1")),
    
    column(3, plotOutput("wcplot2")),
    
    column(3, plotOutput("wcplot3")),
    
    column(3, plotOutput("wcplot4"))
  )
  
)

# 2. Server ----
server <- function(input, output, session) {
  
  
  # Filter the data as selected by user 
  fallzahlen_akut <- reactive({
    
    appdata %>% 
      filter(str_detect(Akt, paste(input$Akt, collapse = "|")),
             Ertrag > input$Ertrag_min*1000000)
  })
  
  # prepare the cluster data
  clusterdata <- reactive({
    
    fallzahlen_akut() %>% 
      select(Spitäler, Bez, Fallzahlen) %>% 
      group_by(Spitäler, Bez) %>% 
      summarise(Fallzahlen = sum(Fallzahlen)) %>% 
      group_by(Spitäler) %>%
      mutate(Fallzahlen_prop = Fallzahlen/sum(Fallzahlen)) %>% 
      ungroup() %>%
      select(Spitäler, Bez, Fallzahlen_prop) %>%
      distinct() %>%
      spread(Bez, Fallzahlen_prop) %>%
      replace(., is.na(.), as.double(0))
    
  })
  
  distancematrix <- reactive({
    
    clusterdata() %>% 
      select(-Spitäler) %>% 
      dist(diag = T)
    
  })
  
  clustering <- reactive({
    
    pam(distancematrix(), k = input$k)$clustering %>% 
      as.factor() %>% 
      as.data.frame() %>% 
      as_tibble() %>%
      rename(cluster = 1) %>% 
      bind_cols(clusterdata() %>% select(Spitäler)) %>% 
      select(Spitäler, cluster)
    
  })
  
  multidimscaled <- reactive({
    
    distancematrix() %>%
      cmdscale() %>%
      as.data.frame() %>%
      as_tibble() %>% 
      bind_cols(clusterdata() %>% select(Spitäler)) %>% 
      select(Spitäler, V1, V2) %>% 
      left_join(clustering(), by = "Spitäler") %>% 
      left_join(fallzahlen_akut() %>% 
                  select(Spitäler, Ertrag) %>%
                  distinct(),
                by = "Spitäler")
    
  })
  
  wordcluster <- reactive({
    
    fallzahlen_akut() %>% 
      select(Spitäler, Fallzahlen, Bez) %>% 
      # filter(Fallzahlen > 300) %>% 
      left_join(clustering(), by = "Spitäler") %>% 
      group_by(cluster, Bez) %>% 
      summarise(Faelle = sum(Fallzahlen)) %>% 
      ungroup() %>% 
      unnest_tokens(Wort, Bez, to_lower = F) %>% 
      group_by(cluster, Wort) %>% 
      summarise(Faelle = sum(Faelle)) %>% 
      ungroup() %>% 
      filter(!Wort %in% stopwords("german"),
             !str_detect(Wort, "[0-9]"),
             str_length(Wort) > 2) %>% 
      #mutate(Wort = wordStem(Wort, "german")) %>% 
      group_by(cluster, Wort) %>% 
      summarise(Faelle = sum(Faelle)) %>% 
      ungroup()
    
  })
  
  output$myplot <- renderggiraph({
    
    multidimscaled() %>% 
      ggplot(aes(V1, V2, color = cluster, size = Ertrag)) + 
      geom_point_interactive(alpha = 0.6, aes(tooltip = Spitäler %>% str_remove_all("\'"))) +
      # coord_fixed(ratio = 1, xlim = c(-3,1.5)) + 
      labs(x = "Component 1", y = "Component 2", color="Cluster",
           title = "k-medoids Cluster", subtitle = "Datenjahr 2016, k = 4",
           caption = "Quelle: BAG Fallzahlen & Spitalstatistik") +
      #geom_text_repel(aes(label = Spitäler), size = 3) +
      #geom_text_repel(aes(label = if_else(Ertrag > 500000000, Spitäler, "")), size = 3) +
      scale_size(guide="none") +
      scale_color_manual(values = mycolors[1:input$k]) + 
      theme_bw() -> gg
    
    ggiraph(code = print(gg), width = 1)
    
  })
  
  output$wcplot1 <- renderPlot({
    
    c <- 1
    par(mar=c(1,1,1,1))
    words <- wordcluster() %>% filter(cluster == c) %>% select(Wort) %>% pull()
    freq <- wordcluster() %>% filter(cluster == c) %>% select(Faelle) %>% pull()
    
    cols.alternating <- rep(c("#555751", mycolors[c]), nwords)
    
    wordcloud(words = words, 
              freq = freq,
              max.words = nwords,
              colors = cols.alternating,
              rot.per = rot,
              scale = cex.scale,
              random.order = F)
    
  })
  
  output$wcplot2 <- renderPlot({
    
    c <- 2
    par(mar=c(1,1,1,1))
    req(input$k >= c)
    
    words <- wordcluster() %>% filter(cluster == c) %>% select(Wort) %>% pull()
    freq <- wordcluster() %>% filter(cluster == c) %>% select(Faelle) %>% pull()
    
    cols.alternating <- rep(c("#555751", mycolors[c]), nwords)
    
    wordcloud(words = words, 
              freq = freq,
              max.words = nwords,
              colors = cols.alternating,
              rot.per = rot,
              scale = cex.scale,
              random.order = F)
    
  })
  
  output$wcplot3 <- renderPlot({
    
    c <- 3
    par(mar=c(1,1,1,1))
    req(input$k >= c)
    
    words <- wordcluster() %>% filter(cluster == c) %>% select(Wort) %>% pull()
    freq <- wordcluster() %>% filter(cluster == c) %>% select(Faelle) %>% pull()
    
    cols.alternating <- rep(c("#555751", mycolors[c]), nwords)
    
    wordcloud(words = words, 
              freq = freq,
              max.words = nwords,
              colors = cols.alternating,
              rot.per = rot,
              scale = cex.scale,
              random.order = F)
    
  })
  
  output$wcplot4 <- renderPlot({
    
    c <- 4
    par(mar=c(1,1,1,1))
    req(input$k >= c)
    
    words <- wordcluster() %>% filter(cluster == c) %>% select(Wort) %>% pull()
    freq <- wordcluster() %>% filter(cluster == c) %>% select(Faelle) %>% pull()
    
    cols.alternating <- rep(c("#555751", mycolors[c]), nwords)
    
    wordcloud(words = words, 
              freq = freq,
              max.words = nwords,
              colors = cols.alternating,
              rot.per = rot,
              # scale = cex.scale,
              random.order = F)
    
  })
  
}

shinyApp(ui, server)