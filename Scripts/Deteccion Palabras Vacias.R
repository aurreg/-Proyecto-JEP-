# Librerias
# lematizador udpipe
library(udpipe)
#
library(stringr)
library(tibble)
library(readr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(igraph)
library(ngram)
library(xtable)
# Leer los subtitulos automatico de la lista del caso 03 de la JEP de youtbe

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"",
                          comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)

stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")

jep03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/jep03.txt", col_names = FALSE)

jep03<-jep03$X1  

subtitulos_JEP_03<-subtitulos_JEP_03%>%
  filter(Titulo %in% jep03)%>%
  as.data.frame()

subcasos<-c('Casanare','Huila','Caribe','Antioquia','Huila')

# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})

#correr esta linea para hacer subcasos
#subtitulos_JEP_03<-subset(subtitulos_JEP_03,str_detect(Titulo,subcasos[1]))



# Preprocesar los subtítulos


# Tokenizar todos los subtítulos
tokenized_subtitles <- subtitulos_JEP_03 %>%
  unnest_tokens(input = Subtitulos, output = word) %>%
  filter(!is.na(word))

# Eliminar stopwords

tokenized_subtitles <- tokenized_subtitles %>%
  anti_join(stop_words_es, by = "word")


# hacer tokenizacion conservando el nombre del video del que procede el video 

tokenized_subtitles <- tokenized_subtitles %>%
  mutate(word = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                       new = replacement_list %>% str_c(collapse = ''),
                       x = word))

# udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez

# Cargar el modelo de idioma español
model = udpipe_load_model(file = "C:/Users/Pc/Documents/spanish-gsd-ud-2.5-191206.udpipe")

# Anotar todos los subtítulos utilizando udpipe
# esto se puede hacer por video pero tarda mucho
tidy_subtitles_annotated = udpipe_annotate(model, 
                                           x = tokenized_subtitles$word, 
                                           doc_id = tokenized_subtitles$Titulo)


# AQUI AQUI 
tidy_subtitles_annotated = as_tibble(tidy_subtitles_annotated)

# Renombrar la columna para tener el token
names(tidy_subtitles_annotated)[6] = "Token"

# Eliminar stopwords adicionales y puntuación
tidy_subtitles_annotated <- tidy_subtitles_annotated %>% 
  # Eliminar puntuación
  mutate(Token = str_remove_all(Token, "[[:punct:]]")) %>%
  # Reemplazar Token con lemma si está disponible
  mutate(Token = case_when(
    !is.na(lemma) ~ lemma,
    TRUE ~ Token
  )) %>%
  # Filtrar Tokens vacíos
  filter(Token != "")


# Analisis frecuencia lemmas
#######

palabras_agrupadas<-tidy_subtitles_annotated %>% 
  count(Token, sort = TRUE)%>%
  as.data.frame()

write.csv(palabras_agrupadas, "palabras_agrupadas.csv", row.names = FALSE)