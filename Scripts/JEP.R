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

tidy_subtitles_annotated = as_tibble(tidy_subtitles_annotated)

# Renombrar la columna para tener el token
names(tidy_subtitles_annotated)[6] = "Token"

# Eliminar stopwords adicionales y puntuación
tidy_subtitles_annotated = tidy_subtitles_annotated %>% 
  anti_join(stop_words_es, by = c("Token" = "word")) %>% 
  mutate(Token = str_remove_all(Token, "[[:punct:]]")) %>% 
  filter(Token != "")

# Analisis frecuencia lemmas
#######
tidy_subtitles_annotated %>% 
  count(lemma, sort = TRUE) %>%
  head(n = 10)

color_inicial <- "#008080"
color_final <- "#011f4b"


p1 <- tidy_subtitles_annotated  %>%
  count(lemma, sort = TRUE) %>%
  filter(n > 20000) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  ggplot(aes(x = lemma, y = n, fill = n)) +  # Usar 'n' para definir el degradado
  theme_light() + 
  geom_col(alpha = 1) +  # No es necesario especificar fill aquí
  scale_fill_gradient(low = color_inicial, high = color_final) +  # Agregar el degradado de color
  xlab(NULL) +
  ylab("Frecuencia") +
  coord_flip() +
  ggtitle(label = 'CASO 3: Palabras más frecuentes (Freq > 20 MIl)') +
  theme(plot.title = element_text(hjust = 0.5))

p1


## NUBE DE PALABRAS

Col = c("#14213d")

# Definir el tamaño del layout con la función png()
tidy_subtitles_annotated %>%
  count(lemma, sort = TRUE) %>%
  with(wordcloud(words = lemma, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

# Añadir título
title(main = "NUBE DE PALABRAS - CASO 3")


## FRECUENCIAS RELATIVAS POR PALABRAS

tidy_subtitles_annotated %>%
  mutate(author = "CASO 3") %>%  
  count(author, lemma) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

decir<-tidy_subtitles_annotated%>%filter(lemma=='acar')
tail(decir$Token)

frecuencia<-tidy_subtitles_annotated%>%group_by(lemma)%>%summarise(frecuencia=n())
frecuencia<-frecuencia%>%arrange(desc(frecuencia))


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


# Analisis de sentimiento 
# Importacion diccionario AFINN
lexico_afinn <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/lexico_afinn.csv", 
                         col_types = cols(word = col_skip()))

# Importacion de diccionario con palabras negativas y positivas

positive_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/positive_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Positivo")
negative_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/negative_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")

sentiment_words <- bind_rows(positive_words, negative_words)


# hacer join con los lemmas obtenidos por el lematizador de palabras
tidy_subtitles_annotated<-tidy_subtitles_annotated%>%left_join(lexico_afinn,by=c('lemma'='palabra'))


# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN<-tidy_subtitles_annotated%>%select(doc_id,lemma,puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN<-AFIN%>%left_join(sentiment_words, by=c('lemma'='word'))

# dejar unicamente las filas que no tengan NA en AFINN(puntuacion) o sentiment
# con el filter se añade nuevos valores a sentiment 
# si la puntuacion es mayor a 0 y hay un NA es una palabra positiva
# si es menor es negativa
# si es igual es neutra
# luego si no hay NA en sentiment se deja la categorizacion que habia en un principio
AFIN<-AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))

# se cuenta el numero de palabras negativas y positivas
# asi como el promedio de las puntuaciones
AFIN2<-AFIN%>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia=n())
AFIN2%>%filter(sentiment=='Neutro')

AFIN_negativo<-AFIN2%>%filter(sentiment=='Negativo' & !is.na(media))
AFIN_positivo<-AFIN2%>%filter(!is.na(media))%>%
  filter(sentiment=='Positivo')%>%
  right_join(AFIN_negativo,by=c('doc_id'='doc_id'))%>%
  select(sentiment=sentiment.x, media=media.x,frecuencia=frecuencia.x)

par(mfrow = c(1, 2))

# Tracer el histograma y la densidad de las proporciones de sentimientos positivos
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 6), xlim = c(0.8, 2.5 ),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)
#abline( v = median(MP), col = "black", lwd = 2)

# Tracer el histograma y la densidad de las proporciones de sentimientos negativos
hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.5), xlim = c(1.2,3.3 ),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
#abline( v = median(-MN), col = "black", lwd = 2)

# Restaurar el diseño de gráficos predeterminado
par(mfrow = c(1, 1))
#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP=AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  labs(x = "Observaciones", y = "Valor", title = "SECUENCIA PARA LOS SENTIMIENTOS \n NEGATIVOS Y POSITIVOS") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))
#### PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)


# PRUEBAS DE DIFERENCIA DE MEDIAS

# Realizar las pruebas y guardar los resultados en una tabla
Pruebas <- data.frame(
  Prueba = c("Mann-Whitney-Wilcoxon", "Suma de rangos con signo de Wilcoxon"),
  W = numeric(2),
  Pvalor = numeric(2)
)

# Prueba de Mann-Whitney-Wilcoxon
wilcoxon_test <- wilcox.test(MN_negativo, MP, alternative = "greater")
Pruebas[1, c("W", "Pvalor")] <- c(wilcoxon_test$statistic, wilcoxon_test$p.value)

# Prueba de suma de rangos con signo de Wilcoxon
wilcoxon_signed_rank_test <- wilcox.test(MN_negativo, MP, alternative = "greater", paired = TRUE)
Pruebas[2, c("W", "Pvalor")] <- c(wilcoxon_signed_rank_test$statistic, wilcoxon_signed_rank_test$p.value)

print(Pruebas)

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla


## PARA LA PROPORCION


## ------------------------------- PROPORCION GRAFICA 1 
PP<-AFIN_positivo$frecuencia
PN<-AFIN_negativo$frecuencia
# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(size = 0.8) +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad",
       title = "DENSIDADES DE LAS PROPORCIONES DE \n LOS SENTIMIENTOS POSITIVOS Y NEGATIVOS") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text("Sentimientos"),
        legend.key.size = unit(1.2, "lines"),
        legend.key = element_rect(color = "transparent"),
        plot.title = element_text(hjust = 0.5))

## PRUEBAS DE BONDAD DE AJUSTE

# Realizar pruebas de normalidad para PP y PN
shapiro_test_PP <- shapiro.test(PP)
shapiro_test_PN <- shapiro.test(PN)

# Imprimir resultados de las pruebas de normalidad
print("Prueba de normalidad para PP:")
print(shapiro_test_PP)

print("Prueba de normalidad para PN:")
print(shapiro_test_PN)

# PRUEBA DE DIFERENCIA DE PROPORCIONES

# Realizar la prueba de Mann-Whitney-Wilcoxon para comparar las Proporciones de PP y PN
mann_whitney_test_PP_PN <- wilcox.test(PN, PP, alternative = "greater")

# Imprimir el resultado de la prueba de Mann-Whitney-Wilcoxon
print("Prueba de Mann-Whitney-Wilcoxon para PN y PP:")
print(mann_whitney_test_PP_PN)

