library(udpipe)
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
library(EnvStats)
library(RColorBrewer)

# Set working directory to data directory
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data")

# Read subtitles and stop words
subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")
stop_words_es <- read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote = "\"", comment.char = "")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

# Read and process corrections
correcciones <- readLines("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Grupos de Palabras/palabras_correcciones.txt")
correcciones_df <- as.data.frame(do.call(rbind, strsplit(correcciones, " - ")), stringsAsFactors = FALSE)
names(correcciones_df) <- c("Palabra_Original", "Correccion")
correcciones_df <- correcciones_df %>%
  mutate(Correccion = str_replace(Correccion, "^NA$", "")) %>%
  filter(Correccion == "")

# Combine stop words
stop_words_es <- c(stop_words_es$V1, FilPal$X1, correcciones_df$Palabra_Original)
stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")

# Preprocess subtitles: remove music tags and NA values
subtitulos_JEP_03 <- subtitulos_JEP_03 %>%
  mutate(Subtitulos = str_replace_all(Subtitulos, "\\[Música\\]", "")) %>%
  filter(!is.na(Subtitulos)) %>%
  as.data.frame()

# Set working directory for bigram outputs
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Outputs/Bigrams/General/")

# Preprocess subtitles: convert to lowercase, remove non-alphabetic characters, and trim whitespace
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")
  Subtitulos <- str_squish(Subtitulos)
})

# Combine subtitles into a single string
subtitulos <- paste(subtitulos_JEP_03$Subtitulos, collapse = ' ')
subtitulos <- tibble(text = subtitulos)

# Tokenize and remove stop words
subtitulos <- subtitulos %>%
  unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+") %>%
  anti_join(x = ., y = stop_words_es)

# Extract tokens
tokens <- subtitulos$word

# Create bigrams
bigrams <- sapply(1:(length(tokens) - 1), function(i) {
  words <- sort(c(tokens[i], tokens[i + 1]))
  paste(words, collapse = " ")
})

# Convert bigrams to dataframe
bigrams <- as.vector(bigrams)
bigrams <- str_split_fixed(bigrams, " ", 2)
word_1 <- bigrams[, 1]
word_2 <- bigrams[, 2]
bigrams2 <- cbind(word_1, word_2)
bigrams2 <- as.data.frame(bigrams2)

# Filter bigrams and count occurrences
bigrams2 <- bigrams2 %>%
  filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word) %>%
  filter(word_1 != word_2) %>%
  count(word_1, word_2, sort = TRUE) %>%
  rename(weight = n)

# Calculate skewness for different thresholds
threshold <- unique(bigrams2$weight)
count <- bigrams2$weight
s <- NULL
for (i in 1:length(threshold)) {
  s[i] <- skewness(bigrams2[count > threshold[i], ]$weight)
}

# Create graph from bigrams
n <- 20
g <- bigrams2 %>%
  filter(weight > n) %>%
  select(word_1, word_2) %>%
  graph_from_data_frame(directed = FALSE)

# Find largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]
g2 <- igraph::induced_subgraph(g, vert_ids)
summary(g2)

# Summarize network
num_nodes <- vcount(g2)
num_edges <- ecount(g2)
N_Value = n


# Community detection and modularity calculation
set.seed(42)
community_methods <- list(
  Edge_Betweenness = cluster_edge_betweenness,
  Fast_Greedy = cluster_fast_greedy,
  Infomap = cluster_infomap,
  Label_Propagation = cluster_label_prop,
  Leading_Eigen = cluster_leading_eigen,
  Leiden = cluster_leiden,
  Louvain = cluster_louvain,
  Spinglass = cluster_spinglass,
  Walktrap = cluster_walktrap
)


modularity_values <- numeric(length(community_methods))
names(modularity_values) <- names(community_methods)

for (method_name in names(community_methods)) {
  tryCatch({
    community_detection <- community_methods[[method_name]](g2)
    modularity_values[method_name] <- modularity(community_detection)
  }, error = function(e) {
    cat("Error with", method_name, ": ", e$message, "\n")
    modularity_values[method_name] <- NA
  })
}
best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values, na.rm = TRUE)


best_modularity <- max(modularity_values, na.rm = TRUE)

n <- 30
g <- bigrams2 %>%
  filter(weight > n) %>%
  select(word_1, word_2) %>%
  graph_from_data_frame(directed = FALSE)

# Find largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]
g2 <- igraph::induced_subgraph(g, vert_ids)
summary(g2)

# Summarize network
num_nodes2 <- vcount(g2)
num_edges2 <- ecount(g2)
N_Value2 = n


# Community detection and modularity calculation
set.seed(42)
community_methods <- list(
  Edge_Betweenness = cluster_edge_betweenness,
  Fast_Greedy = cluster_fast_greedy,
  Infomap = cluster_infomap,
  Label_Propagation = cluster_label_prop,
  Leading_Eigen = cluster_leading_eigen,
  Leiden = cluster_leiden,
  Louvain = cluster_louvain,
  Spinglass = cluster_spinglass,
  Walktrap = cluster_walktrap
)

modularity_values <- numeric(length(community_methods))
names(modularity_values) <- names(community_methods)

for (method_name in names(community_methods)) {
  tryCatch({
    community_detection <- community_methods[[method_name]](g2)
    modularity_values[method_name] <- modularity(community_detection)
  }, error = function(e) {
    cat("Error with", method_name, ": ", e$message, "\n")
    modularity_values[method_name] <- NA
  })
}
best_method <- names(which.max(modularity_values))
best_modularity2 <- max(modularity_values, na.rm = TRUE)

n <- 40
g <- bigrams2 %>%
  filter(weight > n) %>%
  select(word_1, word_2) %>%
  graph_from_data_frame(directed = FALSE)

# Find largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]
g2 <- igraph::induced_subgraph(g, vert_ids)
summary(g2)

# Summarize network
num_nodes3 <- vcount(g2)
num_edges3 <- ecount(g2)
N_Value3 = n


# Community detection and modularity calculation
set.seed(42)
community_methods <- list(
  Edge_Betweenness = cluster_edge_betweenness,
  Fast_Greedy = cluster_fast_greedy,
  Infomap = cluster_infomap,
  Label_Propagation = cluster_label_prop,
  Leading_Eigen = cluster_leading_eigen,
  Leiden = cluster_leiden,
  Louvain = cluster_louvain,
  Spinglass = cluster_spinglass,
  Walktrap = cluster_walktrap
)

modularity_values <- numeric(length(community_methods))
names(modularity_values) <- names(community_methods)

for (method_name in names(community_methods)) {
  tryCatch({
    community_detection <- community_methods[[method_name]](g2)
    modularity_values[method_name] <- modularity(community_detection)
  }, error = function(e) {
    cat("Error with", method_name, ": ", e$message, "\n")
    modularity_values[method_name] <- NA
  })
}

best_method <- names(which.max(modularity_values))
best_modularity3 <- max(modularity_values, na.rm = TRUE)


n <- 50
g <- bigrams2 %>%
  filter(weight > n) %>%
  select(word_1, word_2) %>%
  graph_from_data_frame(directed = FALSE)

# Find largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]
g2 <- igraph::induced_subgraph(g, vert_ids)
summary(g2)

# Summarize network
num_nodes4 <- vcount(g2)
num_edges4 <- ecount(g2)
N_Value4 = n


# Community detection and modularity calculation
set.seed(42)
community_methods <- list(
  Edge_Betweenness = cluster_edge_betweenness,
  Fast_Greedy = cluster_fast_greedy,
  Infomap = cluster_infomap,
  Label_Propagation = cluster_label_prop,
  Leading_Eigen = cluster_leading_eigen,
  Leiden = cluster_leiden,
  Louvain = cluster_louvain,
  Spinglass = cluster_spinglass,
  Walktrap = cluster_walktrap
)

modularity_values <- numeric(length(community_methods))
names(modularity_values) <- names(community_methods)

for (method_name in names(community_methods)) {
  tryCatch({
    community_detection <- community_methods[[method_name]](g2)
    modularity_values[method_name] <- modularity(community_detection)
  }, error = function(e) {
    cat("Error with", method_name, ": ", e$message, "\n")
    modularity_values[method_name] <- NA
  })
}

best_method <- names(which.max(modularity_values))
best_modularity4 <- max(modularity_values, na.rm = TRUE)



df <- data.frame(
  N_Value = c(N_Value3, N_Value4, N_Value2, N_Value),
  num_nodes = c(num_nodes3, num_nodes4, num_nodes2, num_nodes),
  num_edges = c(num_edges3, num_edges4, num_edges2, num_edges),
  best_modularity = c(best_modularity3, best_modularity4, best_modularity2, best_modularity)
)

df <- df %>%
  mutate(across(everything(), ~ round(., 3))) %>%
  arrange(across(everything()))

df
