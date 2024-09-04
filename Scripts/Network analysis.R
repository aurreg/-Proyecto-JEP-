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
#######################################################################################

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})

subtitulos_JEP_03<-paste(subtitulos_JEP_03$Subtitulos, collapse = " ")
names(subtitulos_JEP_03) <- NULL
subtitulos_JEP_03 <- tibble(line = 1:length(subtitulos_JEP_03), text = subtitulos_JEP_03)



subtitulos_JEP_03 %>%
  unnest_tokens(tbl = ., input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) -> subtitulos_JEP_03_bi 

subtitulos_JEP_03_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  filter(!word1 %in% stop_words_es$word) %>%
  filter(!word2 %in% stop_words_es$word) %>%
  # añadir un filter que quite multiples edges
  mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word1)) %>%
  mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word2)) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> bigrams 


bigrams<-text_petro_bi_counts

# Calculate skewness for different thresholds
threshold <- unique(bigrams$weight)
count <- bigrams$weight
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(bigrams[count > threshold[i], ]$weight)
  #hist(bigrams[count > threshold[i], ]$n)
  
}

# Plot skewness vs threshold
plot(threshold, s, 
     xlim = c(0, 100), 
     ylim = range(s, na.rm = TRUE),
     type = 'b',                   # Connect points with lines
     pch = 19,                     # Point character
     col = 'blue',                 # Point color
     xlab = 'Threshold',           # X-axis label
     ylab = 'Skewness',            # Y-axis label
     main = 'Skewness vs Threshold',  # Title
     cex.main = 1.5,               # Title size
     cex.lab = 1.2,                # Axis label size
     cex.axis = 1.1,               # Axis tick label size
     cex = 0.5)                    # Point size


#curve(45 / sqrt(x), from = 0.1, to = 100, add = TRUE, col = 'red', lwd = 2)
# Adding grid lines
grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')

# Adding a horizontal line at y=0 for reference
abline(v = 40, col = 'red', lty = 2)

suppressMessages(suppressWarnings(library(igraph)))
g <- bigrams %>%
  filter(weight > 40) %>%
  graph_from_data_frame(directed = FALSE)


# Find the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, vert_ids)

set.seed(42)
# Applying different community detection algorithms on graph g2
kc_edge_betweenness <- cluster_edge_betweenness(g2)
kc_fast_greedy <- cluster_fast_greedy(g2)
kc_infomap <- cluster_infomap(g2)
kc_label_prop <- cluster_label_prop(g2)
kc_leading_eigen <- cluster_leading_eigen(g2)
kc_leiden <- cluster_leiden(g2)
kc_louvain <- cluster_louvain(g2)
kc_spinglass <- cluster_spinglass(g2)
kc_walktrap <- cluster_walktrap(g2)

# Calculating modularity for each community detection method
mod_edge_betweenness <- modularity(kc_edge_betweenness)
mod_fast_greedy <- modularity(kc_fast_greedy)
mod_infomap <- modularity(kc_infomap)
mod_label_prop <- modularity(kc_label_prop)
mod_leading_eigen <- modularity(kc_leading_eigen)
mod_leiden <- modularity(g2, kc_leiden$membership)
mod_louvain <- modularity(kc_louvain)
mod_spinglass <- modularity(kc_spinglass)
mod_walktrap <- modularity(kc_walktrap)

# Creating a named vector of modularity values
modularity_values <- c(
  Edge_Betweenness = mod_edge_betweenness,
  #Fast_Greedy = mod_fast_greedy,
  Infomap = mod_infomap,
  Label_Propagation = mod_label_prop,
  Leading_Eigen = mod_leading_eigen,
  Leiden = mod_leiden,
  Louvain = mod_louvain,
  Spinglass = mod_spinglass,
  Walktrap = mod_walktrap
)

# Finding the method with the highest modularity
best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values)

# Printing the method with the highest modularity
cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

# Display unique memberships of Louvain algorithm
unique(kc_louvain$membership)

# Summarize the graph
summary(g2)

# Extract word and cluster information
word <- V(g2)$name
cluster <- kc_spinglass$membership

# Create dataframe for clusters
cluster <- cbind(word, cluster)
cluster <- as.data.frame(cluster)



# Exporting for analysis with Python and GPT
write.csv(cluster, "clusters.csv", row.names = FALSE)
write.csv(bigrams2, "bigrams.csv", row.names = FALSE)


##### skip gramas 
subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})

subtitulos_JEP_03<-paste(subtitulos_JEP_03$Subtitulos, collapse = " ")
names(subtitulos_JEP_03) <- NULL
subtitulos_JEP_03 <- tibble(line = 1:length(subtitulos_JEP_03), text = subtitulos_JEP_03)


subtitulos_JEP_03 %>%
  unnest_tokens(tbl = ., input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
  filter(!is.na(skipgram)) -> subtitulos_JEP_03_bi 


##### remover unigramas
suppressMessages(suppressWarnings(library(ngram)))
# contar palabras en cada skip-gram
subtitulos_JEP_03_bi$num_words <- subtitulos_JEP_03_bi$skipgram %>% 
  map_int(.f = ~ wordcount(.x))

subtitulos_JEP_03_bi %<>% 
  filter(num_words == 2) %>% 
  select(-num_words)



head(subtitulos_JEP_03_bi, n = 10)

subtitulos_JEP_03_bi %>%
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
  rename(weight = n) -> skipgrams


# Calculate skewness for different thresholds
threshold <- unique(skipgrams$weight)
count <- skipgrams$weight
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(skipgrams[count > threshold[i], ]$weight)
  #hist(skipgrams[count > threshold[i], ]$n)
  
}

# Plot skewness vs threshold
plot(threshold, s, 
     xlim = c(0, 100), 
     ylim = range(s, na.rm = TRUE),
     type = 'b',                   # Connect points with lines
     pch = 19,                     # Point character
     col = 'blue',                 # Point color
     xlab = 'Threshold',           # X-axis label
     ylab = 'Skewness',            # Y-axis label
     main = 'Skewness vs Threshold',  # Title
     cex.main = 1.5,               # Title size
     cex.lab = 1.2,                # Axis label size
     cex.axis = 1.1,               # Axis tick label size
     cex = 0.5)                    # Point size


#curve(45 / sqrt(x), from = 0.1, to = 100, add = TRUE, col = 'red', lwd = 2)
# Adding grid lines
grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')

# Adding a horizontal line at y=0 for reference
abline(v = 40, col = 'red', lty = 2)

suppressMessages(suppressWarnings(library(igraph)))
gs <- skipgrams %>%
  filter(weight > 40) %>%
  graph_from_data_frame(directed = FALSE)

gs<-igraph::simplify(gs) 

# Find the largest connected component
components <- igraph::clusters(gs, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(gs)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
gs2 <- igraph::induced_subgraph(gs, vert_ids)

set.seed(42)
# Applying different community detection algorithms on graph g2
kc_edge_betweenness <- cluster_edge_betweenness(gs2)
kc_fast_greedy <- cluster_fast_greedy(gs2)
kc_infomap <- cluster_infomap(gs2)
kc_label_prop <- cluster_label_prop(gs2)
kc_leading_eigen <- cluster_leading_eigen(gs2)
kc_leiden <- cluster_leiden(gs2)
kc_louvain <- cluster_louvain(gs2)
kc_spinglass <- cluster_spinglass(gs2)
kc_walktrap <- cluster_walktrap(gs2)

# Calculating modularity for each community detection method
mod_edge_betweenness <- modularity(kc_edge_betweenness)
mod_fast_greedy <- modularity(kc_fast_greedy)
mod_infomap <- modularity(kc_infomap)
mod_label_prop <- modularity(kc_label_prop)
mod_leading_eigen <- modularity(kc_leading_eigen)
mod_leiden <- modularity(gs2, kc_leiden$membership)
mod_louvain <- modularity(kc_louvain)
mod_spinglass <- modularity(kc_spinglass)
mod_walktrap <- modularity(kc_walktrap)

# Creating a named vector of modularity values
modularity_values <- c(
  Edge_Betweenness = mod_edge_betweenness,
  Fast_Greedy = mod_fast_greedy,
  Infomap = mod_infomap,
  Label_Propagation = mod_label_prop,
  Leading_Eigen = mod_leading_eigen,
  Leiden = mod_leiden,
  Louvain = mod_louvain,
  Spinglass = mod_spinglass,
  Walktrap = mod_walktrap
)

# Finding the method with the highest modularity
best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values)

# Printing the method with the highest modularity
cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

# Display unique memberships of Louvain algorithm
unique(kc_louvain$membership)

# Summarize the graph
summary(g2)

# Extract word and cluster information
word <- V(g2)$name
cluster <- kc_spinglass$membership

# Create dataframe for clusters
cluster <- cbind(word, cluster)
cluster <- as.data.frame(cluster)



# Exporting for analysis with Python and GPT
write.csv(cluster, "clusters.csv", row.names = FALSE)
write.csv(skipgrams2, "skipgrams.csv", row.names = FALSE)







