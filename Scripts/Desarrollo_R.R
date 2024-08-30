################################################################################
# Script DESARROLLO ANALISIS DE TEXTO CASO 3 - TALLER 4
# Autor: Cesar Augusto Prieto Sarmiento
# Fecha de Creación: 29/03/2024
# Ultima Fecha de Mod: 18/04/2024
# Descripción: Se plantea el desarrollo del caso 3 de la JEP propuesto como caso
#              de estudio por parte del profesor para el taller 4 del curso.
# Versión: 1.4
################################################################################

#### ---------- LIBRERIAS USADAS
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

setwd("C:/Users/HP/Downloads/Taller4")

##### -------- IMPORTACION DE BASE Y LIMPIEZA

# Establecer la variable de entorno para aumentar el tamaño del búfer segun el tamaño
# del archivo a cargar

Sys.setenv(VROOM_CONNECTION_SIZE = 100 * 1024 * 1024)  

# Ahora intenta leer el archivo CSV nuevamente
Caso3 <- read_csv("CASO3.txt", col_names = FALSE, show_col_types = FALSE)

text_Caso3 <- c(Caso3)
class(text_Caso3)

names(text_Caso3) <- NULL  

##### data frame formato tidy

text_Caso3 <- tibble(line = 1:length(text_Caso3), text = text_Caso3)  # tibble en lugar de data_frame
class(text_Caso3)

##### --------- TOKENIZACION

text_Caso3 <- text_Caso3 %>%
  mutate(text = as.character(text)) %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!is.na(word))

class(text_Caso3)
dim(text_Caso3)

#### --------- NORMALIZACION DE TEXTO

## NUMEROS
text_Caso3 %<>%
  filter(!grepl(pattern = '[0-9]', x = word))

dim(text_Caso3)

## STOP WORDS

stop_words_es <- tibble(word = unlist(c(read.table("stopwords-es.txt", quote="\"", comment.char=""))), lexicon = "custom")
dim(stop_words_es)

text_Caso3 %<>% 
  anti_join(x = ., y = stop_words_es)


dim(text_Caso3)

## GUARDAR LA BASE LIMPIA

saveRDS(text_Caso3, file = "text_Caso3.rds")
text_Caso3 <- readRDS("text_Caso3.rds")


#### ------- TOKENS MÁS FRECUENTES

text_Caso3 <- text_Caso3 %>%
  filter(word != "eh")

palabras_filtrar <- readLines("FilPal.txt")

# Asegurarse de que no haya líneas vacías o espacios en blanco
palabras_filtrar <- palabras_filtrar[nzchar(palabras_filtrar)]

# Filtrar las palabras no deseadas del dataset text_Caso3
text_Caso3 <- text_Caso3 %>%
  filter(!word %in% palabras_filtrar)

# Unificar las palabras mencionadas
text_Caso3 <- text_Caso3 %>%
  mutate(
    word_unified = case_when(
      word %in% c("persona", "personas") ~ "persona",
      word %in% c("pregunta", "preguntas") ~ "pregunta",
      word %in% c("llegar", "llega") ~ "llegar",
      word %in% c("caso", "casos") ~ "caso",
      word %in% c("operacional", "operacionales") ~ "operacional",
      TRUE ~ word  # Para el resto de las palabras, mantenlas igual
    )
  )

text_Caso3 %>% 
  count(word_unified, sort = TRUE) %>%
  head(n = 10)

# SE BORRA LUEGO 

#LP <- text_Caso3 %>% 
#  count(word, sort = TRUE) %>%
#  head(n = 250)

# TABLA EN FORMATO DE LATEX CON xtable 

xtable(text_Caso3 %>% 
         count(word, sort = TRUE) %>%
         head(n = 10), caption = "Palabras más Frecuentes",
       label = "tab:palabras_frecuentes")

## HISTOGRAMA DE FRECUENCIAS

# Definir los colores inicial y final del degradado
color_inicial <- "#008080"
color_final <- "#011f4b"

# Crear el gráfico con un degradado de color
p1 <- text_Caso3 %>%
  count(word_unified, sort = TRUE) %>%
  filter(n > 16800) %>%
  mutate(word_unified = reorder(word_unified, n)) %>%
  ggplot(aes(x = word_unified, y = n, fill = n)) +  # Usar 'n' para definir el degradado
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
png("nube_palabras_caso3.png", width = 1000, height = 530) # Define el tamaño del gráfico
text_Caso3 %>%
  count(word_unified, sort = TRUE) %>%
  with(wordcloud(words = word_unified, scale=c(4,1), freq = n, max.words = 100, colors= Col ))

# Añadir título
title(main = " ")

dev.off()  # Cerrar el dispositivo gráfico después de terminar de guardar el gráfico



## FRECUENCIAS RELATIVAS POR PALABRAS

text_Caso3 %>%
  mutate(author = "CASO 3") %>%  
  count(author, word_unified) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion, fill = 0) -> frecuencia

# Visualizar la frecuencia
head(frecuencia, 100)

frecuencia %>%
  arrange(desc(`CASO 3`))

xtable(head(frecuencia %>%
          arrange(desc(`CASO 3`)), 10), digits = 5, 
       caption = "Frecuencia de la Palabras",
       label = "tab:Frecuencia_de_las_Palabras")


#### ---- BIGRAMAS

text_Caso3 <- c(Caso3)
class(text_Caso3)
str(text_Caso3)

names(text_Caso3) <- NULL

text_Caso3 <- tibble(line = 1:length(text_Caso3), text = text_Caso3)  # tibble en lugar de data_frame
class(text_Caso3)
str(text_Caso3)

# Si la variable 'text' no es de tipo caracter, conviértela
text_Caso3 <- text_Caso3 %>%
  mutate(text = as.character(text))

# Generar bigramas
Caso3_Bi <- text_Caso3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Mostrar los primeros 10 bigramas
head(Caso3_Bi$bigram, 10)

## Top 10 bigramas mas frecuentes

Caso3_Bi %>%
  count(bigram, sort = TRUE) %>%
  head(n = 10)

replacement_list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

# Lee el archivo FilPal.txt en un vector de palabras a filtrar
palabras_filtrar <- readLines("FilPal.txt")
palabras_filtrar <- palabras_filtrar[nzchar(palabras_filtrar)]

Caso3_Bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  filter(!word1 %in% stop_words_es$word) %>%
  filter(!word2 %in% stop_words_es$word) %>%
  # Aplica el filtro de palabras del archivo FilPal.txt
  filter(!word1 %in% palabras_filtrar) %>%
  filter(!word2 %in% palabras_filtrar) %>%
  mutate(
    word1 = case_when(
      word1 %in% c("persona", "personas") ~ "persona",
      word1 %in% c("pregunta", "preguntas") ~ "pregunta",
      word1 %in% c("llegar", "llega") ~ "llegar",
      word1 %in% c("caso", "casos") ~ "caso",
      word1 %in% c("operacional", "operacionales") ~ "operacional",
      TRUE ~ word1
    ),
    word2 = case_when(
      word2 %in% c("persona", "personas") ~ "persona",
      word2 %in% c("pregunta", "preguntas") ~ "pregunta",
      word2 %in% c("llegar", "llega") ~ "llegar",
      word2 %in% c("caso", "casos") ~ "caso",
      word2 %in% c("operacional", "operacionales") ~ "operacional",
      TRUE ~ word2
    )
  ) %>%
  mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word1)) %>%
  mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word2)) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> Caso3_Bi_counts


saveRDS(Caso3_Bi_counts, file = "Caso3_Bi_counts.rds")
Caso3_Bi_counts <- readRDS("Caso3_Bi_counts.rds")

dim(Caso3_Bi_counts)

# Boxplot sin excluir valores atípicos
ggplot(Caso3_Bi_counts, aes(x = "", y = weight)) +
  geom_boxplot() +
  labs(title = "Boxplot de la cantidad de series vistas por usuario",
       y = "Cantidad de series vistas") +
  theme_minimal()

# Aplicar la transformación logarítmica a la columna weight
Caso3_Bi_counts <- Caso3_Bi_counts %>%
  mutate(weight_log = log1p(weight))

# Escalar los valores entre 0 y 1
min_value <- min(Caso3_Bi_counts$weight_log)
max_value <- max(Caso3_Bi_counts$weight_log)
Caso3_Bi_counts <- Caso3_Bi_counts %>%
  mutate(weight_scaled = (weight_log - min_value) / (max_value - min_value))

# Boxplot sin excluir valores atípicos
ggplot(Caso3_Bi_counts, aes(x = "", y = weight_log)) +
  geom_boxplot() +
  labs(title = "Boxplot de la cantidad de series vistas por usuario",
       y = "Cantidad de series vistas") +
  theme_minimal()

summary(Caso3_Bi_counts$weight_scaled)

#### DEFINIR RED A PARTIR DE LA FRECUENCIA (weight) DE LOS BIGRAMAS

str(Caso3_Bi_counts)

g <- Caso3_Bi_counts %>%
  filter(weight_log > 6) %>%
  graph_from_data_frame(directed = FALSE)

# Agrupamiento 
components <- igraph::components(g, mode = "strong")
biggest_cluster_id <- which.max(components$csize)

vert_ids <- V(g)[components$membership == biggest_cluster_id]

# subgraph

projection.G <- igraph::induced_subgraph(g, vert_ids)

# Layout's

l <- layout_with_kk(projection.G)
l1 <- layout_with_fr(g)
l2 <- layout_with_graphopt(g)

max(l[,1]);min(l[,1])
max(l[,2]);min(l[,2])

# Dibujar el gráfico con el layout Kamada-Kawai
plot(gcc, layout = layout, vertex.color = 1, vertex.frame.color = 1, 
     vertex.size = 5, vertex.label.color = 'black', vertex.label.cex = 0.3, 
     vertex.label.dist = 1, main = "Umbral = 6")

plot(projection.G, vertex.label = NA, rescale = FALSE, xlim = c(min(l[, 1]), max(l[, 1])), ylim = c(min(l[, 2]), max(l[, 2])),
     layout = l, vertex.color = adjustcolor("purple", 0.2),
     vertex.frame.color = "violetred4", edge.color = adjustcolor("darkblue", 0.7),
     vertex.label.cex = 0.5, vertex.size = (strength(projection.G))/100,  main = "")
## COMPONENTE CONEXA MAS GRANDE DE LA RED

g <- Caso3_Bi_counts %>%
  filter(weight_log > 5) %>%
  graph_from_data_frame(directed = FALSE)

# grafo inducido por la componente conexa

V(g)$cluster <- clusters(graph = g)$membership
gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))

layout_1 <- layout_with_kk(gcc)
layout1_1 <- layout_with_fr(gcc)
layout2_1 <- layout_with_graphopt(gcc)

par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))
# viz 1

plot(gcc, layout = layout2_1, vertex.color = 1, vertex.frame.color = 1, 
     vertex.size = 3, vertex.label = NA)

# viz 2

plot(gcc, layout = layout2_1, vertex.color = adjustcolor('darkolivegreen4', 0.1),
     vertex.frame.color = 'darkolivegreen4', vertex.size = log1p(strength(gcc)),
     vertex.label = NA, edge.width = 3*E(g)$weight_log/max(E(g)$weight_log))
title(main = "Componente conexa", outer = T, line = -1)

par(mfrow = c(1,1))



##### ----- ANALISI DE COMPONENTE CONEXA SKIP-GRAMA

## Distancia media
mean_distance <- mean_distance(gcc)

## Grado medio
mean_degree <- mean(degree(gcc))

## Desviación estándar del grado
degree_sd <- sd(degree(gcc))

## Número de clanes
clan_number <- clique.number(graph = gcc)

## Densidad
density <- graph.density(gcc)

## Transitividad
transitivity <- transitivity(gcc)

## Asortatividad
assortativity <- assortativity_degree(gcc)

# Imprimir los resultados
print("Medidas del grafo:")
print(paste("Distancia media:", mean_distance))
print(paste("Grado medio:", mean_degree))
print(paste("Desviación estándar del grado:", degree_sd))
print(paste("Número del clan:", clan_number))
print(paste("Densidad:", density))
print(paste("Transitividad:", transitivity))
print(paste("Asortatividad:", assortativity))


#### ----- PALABRAS MAS IMPORTANTES

# Calcula las medidas de centralidad
dc <- degree(graph = gcc, normalized = TRUE)
cc <- closeness(graph = gcc, normalized = TRUE)
bc <- betweenness(graph = gcc, normalized = TRUE)
ec <- eigen_centrality(graph = gcc, scale = TRUE)$vector

# Obtiene los 10 nodos más importantes para cada medida
top_10_dc <- sort(dc, decreasing = TRUE)[1:10]
top_10_cc <- sort(cc, decreasing = TRUE)[1:10]
top_10_bc <- sort(bc, decreasing = TRUE)[1:10]
top_10_ec <- sort(ec, decreasing = TRUE)[1:10]

# Crear tablas con los resultados
tabla_dc <- data.frame(Nodo = 1:10, Grado = top_10_dc)
tabla_cc <- data.frame(Nodo = 1:10, Cercanía = top_10_cc)
tabla_bc <- data.frame(Nodo = 1:10, Intermediación = top_10_bc)
tabla_ec <- data.frame(Nodo = 1:10, CentralidadAutovector = top_10_ec)

# Ordenar cada tabla de mayor a menor
tabla_dc <- tabla_dc[order(-tabla_dc$Grado), ]
tabla_cc <- tabla_cc[order(-tabla_cc$Cercanía), ]
tabla_bc <- tabla_bc[order(-tabla_bc$Intermediación), ]
tabla_ec <- tabla_ec[order(-tabla_ec$CentralidadAutovector), ]

# Crear lista de tablas ordenadas
lista_tablas_ordenadas <- list(Grado = tabla_dc, Cercanía = tabla_cc, Intermediación = tabla_bc, CentralidadAutovector = tabla_ec)

# Imprimir la lista de tablas ordenadas
print("Lista de tablas de medidas de centralidad ordenadas de mayor a menor:")
print(lista_tablas_ordenadas)


#### ----- AGRUPAMIENTO

## -- Detección de comunidades con el algoritmo Leading Eigen
cle <- cluster_leading_eigen(gcc)
str(cle)

sizes(cle)

# Establecer un tamaño mínimo para los vértices
min_vertex_size <- 2

# Calcular el tamaño de los vértices con un tamaño mínimo
vertex_sizes <- pmax(min_vertex_size, 1.6*log1p(strength(gcc)))

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])

# viz 1
par( bg = "#f3f6f4" )
set.seed(123)
plot(gcc, 
     layout = layout2_1, 
     vertex.color = adjustcolor(cols[cle$membership], 0.15), 
     vertex.frame.color = cols[cle$membership], 
     vertex.size = vertex_sizes, 
     vertex.label = NA, 
     main = "Clusters para BI-Gramas Caso 3")

################################################################################
#### ---- SKIP-GRAMS

text_Caso3 <- c(Caso3)
class(text_Caso3)
str(text_Caso3)

names(text_Caso3) <- NULL

text_Caso3 <- tibble(line = 1:length(text_Caso3), text = text_Caso3)  # tibble en lugar de data_frame
class(text_Caso3)
str(text_Caso3)

# Si la variable 'text' no es de tipo caracter, conviértela
text_Caso3 <- text_Caso3 %>%
  mutate(text = as.character(text))

text_Caso3%>%
  unnest_tokens(tbl = ., input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
  filter(!is.na(skipgram)) -> Caso3_skip

dim(Caso3_skip)
head(Caso3_skip)

# contar palabras en cada skip-gram
Caso3_skip$num_words <- Caso3_skip$skipgram %>% 
  map_int(.f = ~ wordcount(.x))

dim(Caso3_skip)
head(Caso3_skip, n = 10)

# remover unigramas
Caso3_skip %<>% 
  filter(num_words == 2) %>% 
  select(-num_words)

dim(Caso3_skip)
head(Caso3_skip)

##### omitir stop words
Caso3_skip %>%
  separate(skipgram, c("word1", "word2"), sep = " ") %>%
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  filter(!word1 %in% stop_words_es$word) %>%
  filter(!word2 %in% stop_words_es$word) %>%
  mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word1)) %>%
  mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word2)) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> Caso3_skip_counts

dim(Caso3_skip_counts)
head(Caso3_skip_counts)

#saveRDS(Caso3_skip_counts, file = "Caso3_skip_counts.rds")
Caso3_skip_counts <- readRDS("Caso3_skip_counts.rds")

# Boxplot sin excluir valores atípicos
ggplot(Caso3_skip_counts, aes(x = "", y = weight)) +
  geom_boxplot() +
  labs(title = "Boxplot de la cantidad de series vistas por usuario",
       y = "Cantidad de series vistas") +
  theme_minimal()

# Aplicar la transformación logarítmica a la columna weight
Caso3_skip_counts <- Caso3_skip_counts %>%
  mutate(weight_log = log1p(weight))

# Escalar los valores entre 0 y 1
min_value <- min(Caso3_skip_counts$weight_log)
max_value <- max(Caso3_skip_counts$weight_log)
Caso3_skip_counts <- Caso3_skip_counts %>%
  mutate(weight_scaled = (weight_log - min_value) / (max_value - min_value))

# Boxplot sin excluir valores atípicos
ggplot(Caso3_skip_counts, aes(x = "", y = weight_log)) +
  geom_boxplot() +
  labs(title = "Boxplot de la cantidad de series vistas por usuario",
       y = "Cantidad de series vistas") +
  theme_minimal()

summary(Caso8_skip_counts$weight_log)

##### definir una red a partir de la frecuencia (weight) de los bigramas

g <- Caso3_skip_counts %>%
  filter(weight > 250) %>%
  graph_from_data_frame(directed = FALSE)

g <- igraph::simplify(g)  # importante!


# grafo inducido por la componente conexa
V(g)$cluster <- clusters(graph = g)$membership
gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(clusters(graph = g)$csize)))

layout_2 <- layout_with_kk(gcc)
layout1_2 <- layout_with_fr(gcc)
layout2_2 <- layout_with_graphopt(gcc)

par(mfrow = c(1,2), mar = c(1,1,2,1), mgp = c(1,1,1))

# viz 1
plot(gcc, layout = layout2_2, vertex.color = 1, vertex.frame.color = 1,
     vertex.size = 3, vertex.label = NA)

# viz 2
plot(gcc, layout = layout2_2, vertex.color = adjustcolor('darkolivegreen4', 0.1),
     vertex.frame.color = 'darkolivegreen4', vertex.size = log1p(strength(gcc)),
     vertex.label = NA)

title(main = "Componente conexa", outer = T, line = -1)


##### ----- ANALISI DE COMPONENTE CONEXA SKIP-GRAMA

## Distancia media
mean_distance <- mean_distance(gcc)

## Grado medio
mean_degree <- mean(degree(gcc))

## Desviación estándar del grado
degree_sd <- sd(degree(gcc))

## Número de clanes
clan_number <- clique.number(graph = gcc)

## Densidad
density <- graph.density(gcc)

## Transitividad
transitivity <- transitivity(gcc)

## Asortatividad
assortativity <- assortativity_degree(gcc)

# Imprimir los resultados
print("Medidas del grafo:")
print(paste("Distancia media:", mean_distance))
print(paste("Grado medio:", mean_degree))
print(paste("Desviación estándar del grado:", degree_sd))
print(paste("Número del clan:", clan_number))
print(paste("Densidad:", density))
print(paste("Transitividad:", transitivity))
print(paste("Asortatividad:", assortativity))


#### ----- PALABRAS MAS IMPORTANTES

# Calcula las medidas de centralidad
dc <- degree(graph = gcc, normalized = TRUE)
cc <- closeness(graph = gcc, normalized = TRUE)
bc <- betweenness(graph = gcc, normalized = TRUE)
ec <- eigen_centrality(graph = gcc, scale = TRUE)$vector

# Obtiene los 10 nodos más importantes para cada medida
top_10_dc <- sort(dc, decreasing = TRUE)[1:10]
top_10_cc <- sort(cc, decreasing = TRUE)[1:10]
top_10_bc <- sort(bc, decreasing = TRUE)[1:10]
top_10_ec <- sort(ec, decreasing = TRUE)[1:10]

# Crear tablas con los resultados
tabla_dc <- data.frame(Nodo = 1:10, Grado = top_10_dc)
tabla_cc <- data.frame(Nodo = 1:10, Cercanía = top_10_cc)
tabla_bc <- data.frame(Nodo = 1:10, Intermediación = top_10_bc)
tabla_ec <- data.frame(Nodo = 1:10, CentralidadAutovector = top_10_ec)

# Ordenar cada tabla de mayor a menor
tabla_dc <- tabla_dc[order(-tabla_dc$Grado), ]
tabla_cc <- tabla_cc[order(-tabla_cc$Cercanía), ]
tabla_bc <- tabla_bc[order(-tabla_bc$Intermediación), ]
tabla_ec <- tabla_ec[order(-tabla_ec$CentralidadAutovector), ]

# Crear lista de tablas ordenadas
lista_tablas_ordenadas <- list(Grado = tabla_dc, Cercanía = tabla_cc, Intermediación = tabla_bc, CentralidadAutovector = tabla_ec)

# Imprimir la lista de tablas ordenadas
print("Lista de tablas de medidas de centralidad ordenadas de mayor a menor:")
print(lista_tablas_ordenadas)


#### ----- AGRUPAMIENTO

## -- Detección de comunidades con el algoritmo Fast Greedy
cfg <- cluster_fast_greedy(gcc)
str(cfg)

sizes(cfg)

# Establecer un tamaño mínimo para los vértices
min_vertex_size <- 2

# Calcular el tamaño de los vértices con un tamaño mínimo
vertex_sizes <- pmax(min_vertex_size, 1.6*log1p(strength(gcc)))

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])

# viz 1
par( bg = "#f3f6f4" )
set.seed(123)
plot(gcc, 
     layout = layout2_2, 
     vertex.color = adjustcolor(cols[cfg$membership], 0.15), 
     vertex.frame.color = cols[cfg$membership], 
     vertex.size = vertex_sizes, 
     vertex.label = NA, 
     main = "Clusters para Skip-Gramas Caso 3")

## -- Detección de comunidades con el algoritmo Edge Betweenness
#ceb <- cluster_edge_betweenness(gcc)
#str(ceb)

## -- Detección de comunidades con el algoritmo Leading Eigen
cle <- cluster_leading_eigen(gcc)
str(cle)

sizes(cle)

# Establecer un tamaño mínimo para los vértices
min_vertex_size <- 2

# Calcular el tamaño de los vértices con un tamaño mínimo
vertex_sizes <- pmax(min_vertex_size, 1.6*log1p(strength(gcc)))

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])

# viz 1
par( bg = "#f3f6f4" )
set.seed(123)
plot(gcc, 
     layout = layout2_2, 
     vertex.color = adjustcolor(cols[cle$membership], 0.15), 
     vertex.frame.color = cols[cle$membership], 
     vertex.size = vertex_sizes, 
     vertex.label = NA, 
     main = "Clusters para Skip-Gramas Caso 3")






