library(dplyr)      # data manipulation
library(tidytext)   # text mining
library(igraph)     # network analysis
library(stringr)      # string manipulation
library(readr)       # CSV file reading
library(readxl)      # Excel file reading
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data")

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"",
                          comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)

stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")


# Preprocess the subtitles
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos) # Convert to lowercase
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "") # Remove non-alphabetic characters
  Subtitulos <- str_squish(Subtitulos) # Remove extra whitespace
})



Todos_Titles <- read_excel("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Todos_Titles.xlsx")
Todos_Titles<-Todos_Titles%>%filter(PRESUNTO==1)%>%filter(!is.na(NOMBRE))


Grupos<-unique(Todos_Titles$NOMBRE)

for (i in 1:length(Grupos)) {
  
  # Filter by Key equal to the current group
  filtro <- Todos_Titles %>%
    filter(NOMBRE == Grupos[i])
  
  filtro <- filtro$Titulo
  
  # Get subtitles for the current group
  subtitulos_Grupo_i <- subtitulos_JEP_03 %>%
    filter(Titulo %in% filtro) %>%
    as.data.frame()
  
  # Join all subtitles into a single string
  subtitulos <- paste(subtitulos_Grupo_i$Subtitulos, collapse = ' ')
  
  # Create tibble for tokenization
  subtitulos <- tibble(text = subtitulos) %>%
    unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+")%>%
    anti_join(x = ., y = stop_words_es)
  
  tokens <- subtitulos$word
  
  # Create bigrams and avoid multiple edges
  bigrams <- sapply(1:(length(tokens) - 1), function(i) {
    words <- sort(c(tokens[i], tokens[i + 1]))
    paste(words, collapse = " ")
  })
  
  bigrams <- as.vector(bigrams)
  
  # Convert bigrams to a dataframe
  bigrams <- str_split_fixed(bigrams, " ", 2)
  
  word_1 <- bigrams[, 1]
  word_2 <- bigrams[, 2]
  
  bigrams2 <- cbind(word_1, word_2)
  bigrams2 <- as.data.frame(bigrams2)
  
  # Filter bigrams that do not contain stop words and that the words are not the same
  bigrams2 <- bigrams2 %>%
    filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word) %>%
    filter(word_1 != word_2) %>%
    count(word_1, word_2, sort = TRUE) %>%
    rename(weight = n)
  
  # Create the graph from the bigrams
  g <- bigrams2 %>%
    graph_from_data_frame(directed = FALSE)
  
  nombre_llave <- Grupos[i]
  ruta <- paste0("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Bigrams perpetrators/grafo_", str_replace_all(str_to_lower(nombre_llave), "[^a-záéíóúüñ ]", ""), ".RData")
  
  # Save the graph as an R object
  save(g, file = ruta)
  
  # Confirmation message
  print(paste("Graph of group", str_replace_all(str_to_lower(nombre_llave), "[^a-záéíóúüñ ]", ""), "saved in", ruta))
}
