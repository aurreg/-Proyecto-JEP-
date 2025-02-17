library(dplyr)
library(tidytext)
library(igraph)
library(stringr)
library(readr)
library(readxl)
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data")

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"",
                          comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)

stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")


# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})



Todos_Titles <- read_excel("C:/Users/Pc/Downloads/Todos_Titles.xlsx")
Todos_Titles<-Todos_Titles%>%filter(PRESUNTO==1)


Grupos<-unique(Todos_Titles$NOMBRE)

for (i in 1:length(Grupos)) {
  
  # Filtro por Llave igual al grupo actual
  filtro <- Todos_Titles %>%
    filter(NOMBRE == Grupos[i])
  
  filtro <- filtro$Titulo
  
  # Obtención de subtítulos del grupo actual
  subtitulos_Grupo_i <- subtitulos_JEP_03 %>%
    filter(Titulo %in% filtro) %>%
    as.data.frame()
  
  # Unir todos los subtítulos en un solo string
  subtitulos <- paste(subtitulos_Grupo_i$Subtitulos, collapse = ' ')
  
  # Crear tibble para tokenizar
  subtitulos <- tibble(text = subtitulos) %>%
    unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+")
  
  tokens <- subtitulos$word
  
  # Crear bigramas y evitar aristas múltiples
  bigrams <- sapply(1:(length(tokens) - 1), function(i) {
    words <- sort(c(tokens[i], tokens[i + 1]))
    paste(words, collapse = " ")
  })
  
  bigrams <- as.vector(bigrams)
  
  # Convertir los bigramas a un dataframe
  bigrams <- str_split_fixed(bigrams, " ", 2)
  
  word_1 <- bigrams[, 1]
  word_2 <- bigrams[, 2]
  
  bigrams2 <- cbind(word_1, word_2)
  bigrams2 <- as.data.frame(bigrams2)
  
  # Filtrar bigramas que no contengan stop words y que las palabras no sean iguales
  bigrams2 <- bigrams2 %>%
    filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word) %>%
    filter(word_1 != word_2) %>%
    count(word_1, word_2, sort = TRUE) %>%
    rename(weight = n)
  
  # Crear el grafo desde los bigramas
  g <- bigrams2 %>%
    graph_from_data_frame(directed = FALSE)
  
  nombre_llave <- Grupos[i]
  ruta <- paste0("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Bigramas/grafo_", nombre_llave, ".RData")
  
  # Guardar el grafo como un objeto R
  save(g, file = ruta)
  
  # Mensaje de confirmación
  print(paste("Grafo del grupo", nombre_llave, "guardado en", ruta))
}
