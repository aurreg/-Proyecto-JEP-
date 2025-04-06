# lematizador udpipe
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


# Read sub-case specific subtitles
SubCaso_NS <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/NorteSantander_subtitles.csv") #Norte de Santander

SubCaso_NS_filtered <- SubCaso_NS[!grepl("Antioquia", SubCaso_NS$Titulo) & !is.na(SubCaso_NS$Subtitulos), ]

SubCaso_CC <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/CostaCaribe_subtitles.csv") #Costa Caribe

SubCaso_C <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Casanare_subtitles.csv") # Casanare

SubCaso_M <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Meta_subtitles.csv") # Meta

SubCaso_H <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Huila_subtitles.csv") #Huila

SubCaso_A <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Antioquia_subtitles.csv") #Antioquia



# Select sub-case specific data
SubCaso <-SubCaso_A 

# Define the sub-case and set working directory
Palabra <- 'Antioquia'
setwd(paste0("C:/Users/Pc/Desktop/-Proyecto-JEP-/Outputs/Skipgrams/", Palabra))

# Determine 'n' based on the sub-case
if (Palabra == 'Norte de santander') {
  n <- 1
} else if (Palabra == 'Costa Caribe') {
  n <- 2
} else if (Palabra == 'Casanare') {
  n <- 20
} else if (Palabra == 'Meta') {
  n <- 1
} else if (Palabra == 'Huila') {
  n <- 20
} else if (Palabra == 'Antioquia') {
  n <- 2
}

# Filter subtitles based on sub-case
if (Palabra == 'Norte de santander') {
  subtitulos_JEP_03 <- subtitulos_JEP_03 %>%
    filter(!(Titulo %in% SubCaso$Titulo) & !is.na(Subtitulos)) %>%
    filter(str_detect(Titulo, Palabra)) %>%
    filter(str_detect(Titulo, Palabra) | str_detect(Titulo, 'Catatumbo')) %>%
    as.data.frame()
} else {
  subtitulos_JEP_03 <- subtitulos_JEP_03 %>%
    filter(!(Titulo %in% SubCaso$Titulo) & !is.na(Subtitulos)) %>%
    filter(str_detect(Titulo, Palabra)) %>%
    as.data.frame()
}

# Combine and preprocess subtitles
subtitulos_JEP_03 <- rbind(subtitulos_JEP_03, SubCaso[, -1]) %>%
  as.data.frame() %>%
  mutate(Subtitulos = str_replace_all(Subtitulos, "\\[Música\\]", "")) %>%
  filter(!is.na(Subtitulos))

subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")
  Subtitulos <- str_squish(Subtitulos)
})

# Tokenize and remove stop words
subtitulos <- paste(subtitulos_JEP_03$Subtitulos, collapse = ' ')
subtitulos <- tibble(text = subtitulos)
subtitulos <- subtitulos %>%
  unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+") %>%
  anti_join(x = ., y = stop_words_es)

# Create skipgrams
tokens <- subtitulos$word
skipgrams <- sapply(1:(length(tokens) - 2), function(i) {
  words <- sort(c(tokens[i], tokens[i + 2]))
  paste(words, collapse = " ")
})
skipgrams <- as.vector(skipgrams)
skipgrams <- str_split_fixed(skipgrams, " ", 2)
word_1 <- skipgrams[, 1]
word_2 <- skipgrams[, 2]
skipgrams2 <- cbind(word_1, word_2)
skipgrams2 <- as.data.frame(skipgrams2)

# Filter skipgrams and count occurrences
skipgrams2 <- skipgrams2 %>%
  filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word) %>%
  filter(word_1 != word_2) %>%
  count(word_1, word_2, sort = TRUE) %>%
  rename(weight = n)

# Calculate skewness for different thresholds
threshold <- unique(skipgrams2$weight)
count <- skipgrams2$weight
s <- NULL
for (i in 1:length(threshold)) {
  s[i] <- skewness(skipgrams2[count > threshold[i], ]$weight)
}

# Plot skewness vs threshold
plot(threshold, s,
     xlim = c(0, 50),
     ylim = range(s, na.rm = TRUE),
     type = 'b',
     pch = 19,
     col = 'blue',
     xlab = 'Threshold',
     ylab = 'Skewness',
     main = '',
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1,
     cex = 0.5
)
grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')
abline(v = 3, col = 'red', lty = 2)

# Create graph from skipgrams
g <- skipgrams2 %>%
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
network_summary <- data.frame(
  Num_Nodes = num_nodes,
  Num_Edges = num_edges,
  N_Value = n
)
write.table(network_summary, "network_summary.txt", sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

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

# Apply each community detection method and calculate modularity

for (method_name in names(community_methods)) {
  
  # Try to apply each method and catch any errors
  
  tryCatch({
    
    # Apply the community detection method
    
    community_detection <- community_methods[[method_name]](g2)
    
    
    
    # Calculate modularity
    
    modularity_values[method_name] <- modularity(community_detection)
    
  }, error = function(e) {
    
    # If an error occurs, print the error message and move to the next method
    
    cat("Error with", method_name, ": ", e$message, "\n")
    
    modularity_values[method_name] <- NA
    
  })
  
}

best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values, na.rm = TRUE)

cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

kc <- community_methods[[best_method]](g2)
result_table <- data.frame(
  Best_Method = best_method,
  Modularity_Value = best_modularity,
  Unique_Memberships = max(unique(kc$membership))
)
write.table(result_table, "modularity_results.txt", sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")
summary(g2)

# Cleanup
rm(kc_edge_betweenness, kc_fast_greedy, kc_infomap, kc_label_prop, kc_leading_eigen, kc_leiden, kc_louvain, kc_spinglass, kc_walktrap,
   mod_edge_betweenness, mod_fast_greedy, mod_infomap, mod_label_prop, mod_leading_eigen, mod_leiden, mod_louvain, mod_spinglass, mod_walktrap,
   modularity_values)

# Graph visualizations
cols <- c(brewer.pal(9, "Set1")[1:9], brewer.pal(8, "Set2")[1:7], brewer.pal(8, "Set2")[1:7], brewer.pal(12, "Set3")[1:3])

label <- V(g2)$name
label <- sample(label)
porcentaje <- 0.95
n_reemplazar <- round(length(label) * porcentaje)
set.seed(123)
posiciones <- sample(1:length(label), n_reemplazar)
label[posiciones] <- NA

png("graph.png", width = 800, height = 600)
set.seed(123)
plot(g2, layout = layout_with_kk, vertex.color = adjustcolor('#011f4b', 0.1),
     vertex.frame.color = adjustcolor('#011f4b', 0.5), vertex.size=5,
     vertex.label = NA, edge.color = adjustcolor('gray', 0.2))
dev.off()

png("graphd.png", width = 800, height = 600)
set.seed(123)
plot(g2, layout = layout_with_kk, vertex.color = adjustcolor(cols[kc$membership], 0.1),
     vertex.frame.color = adjustcolor(cols[kc$membership], 0.5), vertex.size = 5,
     vertex.label = NA, edge.color = adjustcolor('gray', 0.2))
dev.off()

# Graph statistics table
tab <- cbind(c("Dist. media", "Grado media", "Grado desviación", "Número clan", "Densidad", "Transitividad", "Asortatividad"),
             round(c(mean_distance(g2), mean(degree(g2)), sd(degree(g2)), clique.number(g2), edge_density(g2),
                     transitivity(g2), assortativity_degree(g2)), 4)
)
write.table(tab, "tab.txt", sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Eigenvector centrality calculation and output
centralidad <- tibble(word = V(g2)$name, eigen = eigen_centrality(g2, scale = TRUE)$vector)
centralidad <- centralidad %>%
  arrange(desc(eigen)) %>%
  head(n = 20)
write.table(centralidad, "centralidad.txt", sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Cluster information output
word <- V(g2)$name
cluster <- kc$membership
eigen <- eigen_centrality(g2, scale = TRUE)$vector
cluster <- cbind(word, cluster, eigen)
cluster <- as.data.frame(cluster)
write.table(cluster, "clusters_jep.txt", sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Top 10 centrality
centralidad %>%
  arrange(desc(eigen)) %>%
  head(n = 10)

# Edge list output
edges <- as_edgelist(g2, names = TRUE)
write.table(edges, file = "edges.txt", row.names = FALSE, col.names = FALSE)
