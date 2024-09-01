library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(udpipe)
library(tibble)

# Leer los datos
subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})

# Tokenizar todos los subtítulos
tokenized_subtitles <- subtitulos_JEP_03 %>%
  unnest_tokens(input = Subtitulos, output = word) %>%
  filter(!is.na(word))

# Eliminar stopwords
stop_words_es <- tibble(word = unlist(c(read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"",
                                                   comment.char=""))), lexicon = "custom")

tokenized_subtitles <- tokenized_subtitles %>%
  anti_join(stop_words_es, by = "word")

# Normalización de acentos
replacement_list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
tokenized_subtitles <- tokenized_subtitles %>%
  mutate(word = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                       new = replacement_list %>% str_c(collapse = ''),
                       x = word))
library(udpipe)
# udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez
# Cargar el modelo de idioma español
model = udpipe_load_model(file = "C:/Users/Pc/Documents/spanish-gsd-ud-2.5-191206.udpipe")

# Anotar todos los subtítulos utilizando udpipe
tidy_subtitles_annotated = udpipe_annotate(model, 
                                           x = tokenized_subtitles$word, 
                                           doc_id = tokenized_subtitles$Titulo)
tidy_subtitles_annotated = as_tibble(tidy_subtitles_annotated)

# Renombrar la columna para tener el token
names(tidy_subtitles_annotated)[6] = "Token"

# Eliminar stopwords adicionales y puntuación
tidy_subtitles_annotated = tidy_subtitles_annotated %>% 
  anti_join(stop_words_es, by = c("Token" = "word")) %>% 
  mutate(Token = str_remove_all(Token, "[[:punct:]]")) %>% 
  filter(Token != "")

# Análisis de Frecuencia por Etiqueta POS
tidy_subtitles_annotated %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  labs(x = "Etiqueta POS", y = "Frecuencia", title = "Subtítulos: Video JEP") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
tidy_subtitles_annotated %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  labs(x = "Lemma", y = "Frecuencia", title = "Subtítulos: Video JEP") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))



lexico_afinn <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/lexico_afinn.csv", 
                         col_types = cols(word = col_skip()))

positive_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/positive_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Positivo")
negative_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/negative_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")
sentiment_words <- bind_rows(positive_words, negative_words)

str(tidy_subtitles_annotated)

tidy_subtitles_annotated<-tidy_subtitles_annotated%>%left_join(lexico_afinn,by=c('Token'='palabra'))

AFIN<-tidy_subtitles_annotated%>%select(doc_id,Token,puntuacion)

AFIN<-AFIN%>%left_join(sentiment_words, by=c('Token'='word'))

AFIN<-AFIN %>%
  filter(!is.na(puntuacion) & !is.na(sentiment)) %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion))

AFIN_negativo<-AFIN%>%filter(sentiment=='Negativo')
plot(1:length(AFIN_negativo$media),AFIN_negativo$media,type='l')


