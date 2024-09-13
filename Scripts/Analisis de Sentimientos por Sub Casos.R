################################################################################
# Script Analisis de sentimientos SubCasos: Proyecto JEP Casos 03 
# Autores: 
# - Alejandro Urrego Lopez
# - Cesar Augusto Prieto Sarmiento
# Fecha de Creación: 03/09/2024
# Ultima Fecha de Mod: 12/09/2024
# Descripción: Se plantea el desarrollo del análisis de sentimientos para el
#             proyecto JEP Casos 03, en el cual se busca identificar la
#             polaridad del discurso realizados por las victimas y victimarios en 
#             los casos de violencia estudiados y de los cuales hay registro del Caso 03
# Versión: 1
################################################################################

# CARGA DE LIBRERIAS ----
library(easypackages)

libraries(c('udpipe', 'stringr', 'tibble', 'readr', 'ggplot2', 'wordcloud', 'dplyr',
            'tidyr', 'tidyverse', 'tidytext', 'magrittr', 'igraph', 'ngram', 'xtable'))

# Leer los subtitulos de los SubCasos del Caso 03 de la JEP 

setwd("C:/Users/Cesar Prieto/Documents/REPOS GIT")
SubCaso_NS <- read_csv("-Proyecto-JEP-/Data/NorteSantander_subtitles.csv") #Subtitulos Para Norte de Santander
SubCaso_CC <- read_csv("-Proyecto-JEP-/Data/CostaCaribe_subtitles.csv") #Subtitulos Para Costa Caribe
SubCaso_C <- read_csv("-Proyecto-JEP-/Data/Casanare_subtitles.csv") # Subtitulos para Casanare
SubCaso_H <- read_csv("-Proyecto-JEP-/Data/Huila_subtitles.csv") #Subtitulos Para Huila
SubCaso_M <- read_csv("-Proyecto-JEP-/Data/Meta_subtitles.csv") #Subtitulos Para Meta
SubCaso_A <- read_csv("-Proyecto-JEP-/Data/Antioquia_subtitles.csv") #Subtitulos Para Antioquia

stop_words_es<-read.table("-Proyecto-JEP-/Data/stop_words_spanish.txt",
                          quote="\"", comment.char="")
FilPal <- read_csv("-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)
stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")

# Lista de dataframes
df_list <- list(SubCaso_NS, SubCaso_CC, SubCaso_H, SubCaso_M, SubCaso_A, SubCaso_C)
names(df_list) <- c('SubCaso_NS', 'SubCaso_CC', 'SubCaso_H', 'SubCaso_M', 'SubCaso_A', 'SubCaso_C')

# Preprocesar los subtítulos
for (i in seq_along(df_list)) {
  df_list[[i]] <- df_list[[i]] %>%
    mutate(
      Subtitulos = str_to_lower(Subtitulos),  # Convertir a minúsculas
      Subtitulos = str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", ""),  # Eliminar caracteres no alfabéticos
      Subtitulos = str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
    )
}

# Asignar los dataframes procesados de vuelta a las variables originales
list2env(df_list, envir = .GlobalEnv)

# Definir la lista de reemplazos (asegúrate de tener esta lista definida)
replacement_list <- c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u", "ñ" = "n")

# Función para procesar un dataframe
process_dataframe <- function(df) {
  df %>%
    unnest_tokens(input = Subtitulos, output = word) %>%
    filter(!is.na(word)) %>%
    anti_join(stop_words_es, by = "word") %>%
    mutate(word = chartr(
      old = names(replacement_list) %>% str_c(collapse = ''),
      new = replacement_list %>% str_c(collapse = ''),
      x = word
    ))
}

# Aplicar el procesamiento a cada dataframe en la lista
for (i in seq_along(df_list)) {
  df_list[[i]] <- process_dataframe(df_list[[i]])
}

# Asignar los dataframes procesados de vuelta a las variables originales
list2env(df_list, envir = .GlobalEnv)

head(SubCaso_NS)

#udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez

# Cargar el modelo de idioma español
model = udpipe_load_model(file = "-Proyecto-JEP-/Scripts/spanish-gsd-ud-2.5-191206.udpipe")

# Lista para almacenar los resultados anotados
annotated_list <- list()

# Aplicar UDPipe a cada dataframe tokenizado
for (name in names(df_list)) {
  print(paste("Processing", name))
  
  # Anotar los subtítulos utilizando udpipe
  annotated <- udpipe_annotate(model, 
                               x = df_list[[name]]$word, 
                               doc_id = df_list[[name]]$Titulo)
  
  # Convertir a dataframe y añadir a la lista
  annotated_list[[paste0(name, "_annot")]] <- as.data.frame(annotated)
}

# Asignar los dataframes anotados a nuevas variables en el entorno global
list2env(annotated_list, envir = .GlobalEnv)

# Lista de dataframes anotados
TS_SubCaso_C = as_tibble(SubCaso_C_annot)
TS_SubCaso_NS= as_tibble(SubCaso_NS_annot)
TS_SubCaso_CC= as_tibble(SubCaso_CC_annot)
TS_SubCaso_H= as_tibble(SubCaso_H_annot)
TS_SubCaso_M= as_tibble(SubCaso_M_annot)
TS_SubCaso_A= as_tibble(SubCaso_A_annot)

# Renombrar la columna para tener el token
names(TS_SubCaso_C)[6] = "Token"
names(TS_SubCaso_NS)[6] = "Token"
names(TS_SubCaso_CC)[6] = "Token"
names(TS_SubCaso_H)[6] = "Token"
names(TS_SubCaso_M)[6] = "Token"
names(TS_SubCaso_A)[6] = "Token"

# Eliminar stopwords adicionales y puntuación
# Lista de nombres de los dataframes anotados
annotated_names <- c("SubCaso_NS_annot", "SubCaso_CC_annot", "SubCaso_H_annot",
                     "SubCaso_M_annot", "SubCaso_A_annot", "SubCaso_C_annot")

# Función para procesar un dataframe anotado
process_annotated_df <- function(df_name) {
  df <- get(df_name) %>% 
    as_tibble() %>%
    rename(Token = token) %>%
    mutate(
      Token = str_remove_all(Token, "[[:punct:]]"),
      Token = case_when(
        !is.na(lemma) ~ lemma,
        TRUE ~ Token
      )
    ) %>%
    filter(Token != "")
  
  return(df)
}

# Aplicar el procesamiento a cada dataframe y guardar los resultados
for (name in annotated_names) {
  new_name <- paste0("TS_", sub("_annot$", "", name))
  assign(new_name, process_annotated_df(name), envir = .GlobalEnv)
}

# Verificar el resultado
head(TS_SubCaso_NS)

## ANALISIS DE LAS FRECUENCIAS DE LOS LEMAS ----

### Sub Caso Casanare ----

TS_SubCaso_C %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_C$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 6000 y seleccionar los top 20
top_tokens <- head(token_counts[token_counts > 6000], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO CASANARE', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

## NUBE DE PALABRAS

Col = c("#14213d")

# Definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_NS %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))


### Sub Caso Norte de Santander ----

TS_SubCaso_NS %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_NS$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 3 y seleccionar los top 20
top_tokens <- head(token_counts[token_counts > 100], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO NORTE DE SANTANDER', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

## NUBE DE PALABRAS

Col = c("#14213d")

# Definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_C %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

### Sub Caso Costa Caribe ----

TS_SubCaso_CC %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_CC$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 300 y seleccionar los top 10
top_tokens <- head(token_counts[token_counts > 300], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO COSTA CARIBE', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

# NUBE DE PALABRAS 

Col = c("#14213d")

# definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_CC %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

### Sub Caso Huila ----

TS_SubCaso_H %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_H$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 100 y seleccionar los top 10
top_tokens <- head(token_counts[token_counts > 100], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO HUILA', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

## NUBE DE PALABRAS

Col = c("#14213d")

# Definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_H %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

### Sub Caso Meta ----

TS_SubCaso_M %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_M$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 13000 y seleccionar los top 10
top_tokens <- head(token_counts[token_counts > 13000], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO META', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

## NUBE DE PALABRAS

Col = c("#14213d")

# Definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_M %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

### Sub Caso Antioquia ----

TS_SubCaso_A %>% 
  count(Token, sort = TRUE) %>%
  head(n = 10)

# Contar frecuencias de tokens
token_counts <- table(TS_SubCaso_A$Token)
token_counts <- sort(token_counts, decreasing = TRUE)

# Filtrar tokens con frecuencia > 300 y seleccionar los top 10
top_tokens <- head(token_counts[token_counts > 300], 10)

# Crear el gráfico de barras
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')  # Ajustar márgenes
barplot(rev(top_tokens),horiz = TRUE, xlim = c(0, max(top_tokens) + 2), las = 1, col = colorRampPalette(c("#008080", "#011f4b"))(length(top_tokens)),
        xlab = "Frecuencia", cex.names = 0.8, family = "sans", font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4,
        main = 'PALABRAS MÁS FRECUENTES PARA\nSUBCASO ANTIOQUIA', cex.axis = 0.7, col.main = 'black',
        sub = 'JEP: Caso 03 - Analisis de discrusos', font.sub = 2, cex.sub = 0.8, col.sub = 'grey25')

## NUBE DE PALABRAS 

Col = c("#14213d")

# Definir el tamaño del layout

par(mar = c(0, 0, 0, 0), bg = 'white')

TS_SubCaso_A %>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

## OTRAS FRECUENCIAS PARA LOS TOKENS ----

### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO CASANARE ----

TS_SubCaso_C %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_C %>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_C %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIAS DE LAS\nPALABRAS SUB CASO CASANARE",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_C %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO CASANARE",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))


### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO NORTE DE SANTANDER ----

TS_SubCaso_NS %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_NS%>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_NS %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIA DE LAS\nPALABRAS SUB CASO NORTE DE SANTANDER",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_NS %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO NORTE DE SANTANDER",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO COSTA CARIBE ----

TS_SubCaso_CC %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_CC%>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_CC %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIA DE LAS\nPALABRAS SUB CASO COSTA CARIBE",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_CC %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO COSTA CARIBE",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO HUILA ----

TS_SubCaso_H %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_H %>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_H %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIA DE LAS\nPALABRAS SUB CASO HUILA",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_H %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO HUILA",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO META ----

TS_SubCaso_M %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_M %>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_M %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIA DE LAS\nPALABRAS SUB CASO META",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_M %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO META",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

### FRECUENCIAS RELATIVAS POR PALABRAS PARA SUB CASO ANTIOQUIA ----

TS_SubCaso_A %>%
  mutate(author = "CASO 3") %>%  
  count(author, Token) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 20)

# Análisis de Frecuencia por Etiqueta POS

frecuencia <- TS_SubCaso_A %>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia <- frecuencia%>%
  arrange(desc(frecuencia))

TS_SubCaso_A %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  ggtitle(label = "CLASIFICACION Y FRECUENCIA DE LAS\nPALABRAS SUB CASO ANTIOQUIA",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Etiqueta POS", y = "Frecuencia") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
TS_SubCaso_A %>% 
  filter(upos %in% c('NOUN', 'PROPN', 'VERB', 'ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(lemma, n, upos), y = n, fill = lemma)) +
  scale_x_reordered() +
  ggtitle(label = "TOP 5 LEMAS POR CATEGORIA GRAMATICAL\nSUB CASO ANTIOQUIA",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Lemma", y = "Frecuencia") +
  facet_wrap(vars(upos), scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## ANALISIS DE SENTIMIENTOS ----

# Importacion diccionario AFINN
lexico_afinn <- read_csv("-Proyecto-JEP-/Data/lexico_afinn.csv",col_types = cols(word = col_skip()))

# como es una traduccion hay palabras repetidas por lo tanto se hace un promedio de los puntajes
lexico_afinn <- lexico_afinn %>%
  group_by(palabra) %>% 
  summarise(puntuacion = mean(puntuacion))

# Importacion de diccionario con palabras negativas y positivas

positive_words <- read_csv("-Proyecto-JEP-/Data/positive_words_es.txt", 
                           col_names = "word", show_col_types = FALSE) %>% 
  mutate(sentiment = "Positivo")

negative_words <- read_csv("-Proyecto-JEP-/Data/negative_words_es.txt",
                           col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")

sentiment_words <- bind_rows(positive_words, negative_words)

### ANALISIS DE SENTIMIENTOS PARA SUB CASO CASANARE ----

TS_SubCaso_C2 <- TS_SubCaso_C %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_C2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_C2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.6), xlim = c(0.7, 2.2),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.8), xlim = c(1.2, 2.6),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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


### ANALISIS DE SENTIMIENTOS PARA SUB CASO NORTE DE SANTANDER ----

TS_SubCaso_NS2 <- TS_SubCaso_NS %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_NS2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_NS2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.2), xlim = c(0, 3),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 3.5), xlim = c(1.7, 2.5),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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

### ANALISIS DE SENTIMIENTOS PARA SUB CASO COSTA CARIBE ----

TS_SubCaso_CC2 <- TS_SubCaso_CC %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_CC2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_CC2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias\nde sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.5), xlim = c(0.5, 2.5),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias\nde sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 4), xlim = c(1, 2.6),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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

### ANALISIS DE SENTIMIENTOS PARA SUB CASO HUILA ----

TS_SubCaso_H2 <- TS_SubCaso_H %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_H2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_H2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.5), xlim = c(0.5, 2.5),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 4), xlim = c(1, 2.6),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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

### ANALISIS DE SENTIMIENTOS PARA SUB CASO META ----

TS_SubCaso_M2 <- TS_SubCaso_M %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_M2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_M2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.5), xlim = c(0.5, 2.5),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 4), xlim = c(1, 2.6),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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

### ANALISIS DE SENTIMIENTOS PARA SUB CASO ANTIOQUIA ----

TS_SubCaso_A2 <- TS_SubCaso_A %>% 
  left_join(lexico_afinn, by = c('Token'='palabra'))

str(TS_SubCaso_A2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN <- TS_SubCaso_A2 %>% 
  select(doc_id, Token, puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN <- AFIN %>% 
  left_join(sentiment_words, by = c('Token'='word'))

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(sentiment=case_when(puntuacion > 0 & is.na(sentiment) ~ "Positivo",
                             puntuacion < 0 & is.na(sentiment) ~ "Negativo",
                             puntuacion == 0 & is.na(sentiment) ~ "Neutro",
                             TRUE ~ sentiment))
AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion,na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>%
  filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>% 
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>% 
  as.data.frame()

par(mfrow = c(1, 2))
hist(AFIN_positivo$media, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 6), xlim = c(1.4, 2.1),
     border = "transparent")
lines(density(AFIN_positivo$media), col = "4", lwd = 2)

hist((-1)*AFIN_negativo$media, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 25), xlim = c(2, 2.2),
     border = "transparent")
lines(density((-1)*AFIN_negativo$media), col = "2", lwd = 2)
par(mfrow = c(1, 1))

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

names(MN_negativo) <- NULL

# Crear un data frame para ggplot

MN_negativo[6] <- mean(MN_negativo)
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  ggtitle(label = "SECUENCIA PARA LOS SENTIMIENTOS NEGATIVOS Y POSITIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Observaciones", y = "Valor") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

## PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA DE LOS SENTIMIENTOS

# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)
print("Prueba de normalidad para MP:")
print(shapiro_test_MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test( MN_negativo)
print("Prueba de normalidad para MN:")
print(shapiro_test_MN)

# PRUEBAS DE DIFERENCIA DE MEDIAS

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

Tabla <- cbind(Media = c(mean(MN_negativo), mean(MP)), 
               Mediana = c(median(MN_negativo),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla

## PARA LA PROPORCION

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia
PN <- AFIN_negativo$frecuencia
PN[6] <- mean(AFIN_positivo$frecuencia)


PP <- PP/(PP + PN)
PN <- PN/(PP + PN)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Crear un data frame para las densidades
densidades_df <- data.frame(x = c(densidad_PP$x, densidad_PN$x),
                            y = c(densidad_PP$y, densidad_PN$y),
                            grupo = rep(c("Negativo", "Positivo"), each = length(densidad_PP$x)))

# Gráfico ggplot
ggplot(densidades_df, aes(x = x, y = y, color = grupo)) +
  geom_line(linewidth = 0.8) +
  ggtitle(label = "DENSIDADES DE LAS PROPORCIONES DE\nLOS SENTIMIENTOS POSITIVOS Y NEGATIVOS",
          subtitle = "JEP: Caso 03 - Analisis de discrusos") +
  labs(x = "Proporción de \n sentimientos  ", y = "Densidad") +
  scale_color_manual(name = "Sentimientos", labels = c("Negativo", "Positivo"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), 
        legend.position = "right", text = element_text(size = 10),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey50"))

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


