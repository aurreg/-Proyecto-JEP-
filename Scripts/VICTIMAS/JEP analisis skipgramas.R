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
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Outputs/Skipgramas - Victimas/General")
subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"",
                          comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)


# Leer el archivo línea por línea
correcciones <- readLines("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Grupos de Palabras/palabras_correcciones.txt")

# Separar las palabras originales de sus correcciones
correcciones_df <- as.data.frame(do.call(rbind, strsplit(correcciones, " - ")), stringsAsFactors = FALSE)
names(correcciones_df) <- c("Palabra_Original", "Correccion")
correcciones_df <-correcciones_df%>%
  mutate(Correccion = str_replace(Correccion, "^NA$", ""))%>%
  filter(Correccion=="")


stop_words_es<-c(stop_words_es$V1,FilPal$X1,correcciones_df$Palabra_Original)

stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")


Todos_Titles <- read_excel("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Todos_Titles.xlsx")
Todos_Titles<-Todos_Titles%>%filter(PRESUNTO==0)%>%select(Titulo)
Todos_Titles<-Todos_Titles$Titulo




subtitulos_JEP_03<-subtitulos_JEP_03%>%
  filter(Titulo %in% Todos_Titles)%>%
  mutate(Subtitulos= str_replace_all(Subtitulos, "\\[Música\\]", "")) %>%
  filter(!is.na(Subtitulos))%>%
  as.data.frame()


# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})

#correr esta linea para hacer subcasos


subtitulos= paste(subtitulos_JEP_03$Subtitulos, collapse = ' ')

subtitulos <- tibble(text = subtitulos)

subtitulos <- subtitulos %>%
  unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+")%>%
  anti_join(x = ., y = stop_words_es)

# Extract tokens
tokens <- subtitulos$word

# Create bigrams from tokens and avoid multiple edges
skipgrams <-sapply(1:(length(tokens) - 2), function(i) {
  words <- sort(c(tokens[i], tokens[i + 2]))
  paste(words, collapse = " ")
})

# Convert bigrams to a vector
skipgrams <- as.vector(skipgrams)

# Convert bigrams to dataframe
skipgrams<- str_split_fixed(skipgrams, " ", 2)

word_1 <- skipgrams[, 1]
word_2 <- skipgrams[, 2]

skipgrams2 <- cbind(word_1, word_2)
skipgrams2 <- as.data.frame(skipgrams2)

skipgrams2<-skipgrams2%>%
  filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word)%>%
  filter(word_1 != word_2)%>%
  count(word_1, word_2, sort = TRUE) %>%
  rename(weight = n)

# Calculate skewness for different thresholds
threshold <- unique(skipgrams2$weight)
count <- skipgrams2$weight
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(skipgrams2[count > threshold[i], ]$weight)
  #hist(bigrams[count > threshold[i], ]$n)
  
}

# Plot skewness vs threshold
png("skewness.png", width = 800, height = 600)
plot(threshold, s, 
     xlim = c(0, 100), 
     ylim = range(s, na.rm = TRUE),
     type = 'b',                   # Connect points with lines
     pch = 19,                     # Point character
     col = 'blue',                 # Point color
     xlab = 'Threshold',           # X-axis label
     ylab = 'Skewness',            # Y-axis label
     main = '',  # Title
     cex.main = 1.5,               # Title size
     cex.lab = 1.2,                # Axis label size
     cex.axis = 1.1,               # Axis tick label size
     cex = 0.5)                    # Point size

#curve(45 / sqrt(x), from = 0.1, to = 100, add = TRUE, col = 'red', lwd = 2)
# Adding grid lines
grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')

# Adding a horizontal line at y=0 for reference
abline(v = 40, col = 'red', lty = 2)
dev.off()

suppressMessages(suppressWarnings(library(igraph)))
n<-20
g <- skipgrams2%>%
  filter(weight > n) %>%
  select(word_1,word_2)%>%
  graph_from_data_frame(directed = FALSE)



# Find the largest connected component
components <- igraph::clusters(g, mode = "weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# Create subgraph of the largest component
g2 <- igraph::induced_subgraph(g, vert_ids)


num_nodes <- vcount(g2)
num_edges <- ecount(g2)

# Crear un dataframe para almacenar esta información
network_summary <- data.frame(
  Num_Nodes = num_nodes,
  Num_Edges = num_edges,
  N_Value = n
)
write.table(network_summary, "network_summary.txt",sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

set.seed(42)
# List of community detection functions
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

# Initialize a vector to store modularity values
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


# Find the method with the highest modularity
best_method <- names(which.max(modularity_values))
best_modularity <- max(modularity_values)

# Print the results
cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

# Set the community detection method with the highest modularity
kc <- community_methods[[best_method]](g2)
# Crear un dataframe con la información
result_table <- data.frame(
  Best_Method = best_method,
  Modularity_Value = best_modularity,
  Unique_Memberships = max(unique(kc$membership))
)

# Summarize the graph
write.table(result_table, "modularity_results.txt",sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

summary(g2)

suppressMessages(suppressWarnings(library(RColorBrewer)))

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])
# viz 1
png("graph.png", width = 800, height = 600)
set.seed(123)
plot(g2, layout = layout_with_kk, vertex.color = adjustcolor('#011f4b', 0.1), 
     vertex.frame.color =adjustcolor('#011f4b', 0.5), vertex.size = ifelse(0.8*degree(g2)>1,ifelse(0.8*degree(g2)<15,0.8*degree(g2),15),1), 
     vertex.label = NA, edge.color=adjustcolor('gray',0.8))
dev.off()
png("graphd.png", width = 800, height = 600)
set.seed(123)
plot(g2, layout = layout_with_kk,vertex.color = adjustcolor(cols[kc$membership], 0.1), 
     vertex.frame.color =adjustcolor(cols[kc$membership],0.5), vertex.size = ifelse(0.8*degree(g2)>1,ifelse(0.8*degree(g2)<15,0.8*degree(g2),15),1), 
     vertex.label = NA, edge.color=adjustcolor('gray',0.8))

dev.off()

tab <- cbind(c("Dist. media","Grado media","Grado desviación","Número clan","Densidad","Transitividad","Asortatividad"),
             round(c(mean_distance(g2), mean(degree(g2)), sd(degree(g2)), clique.number(g2), edge_density(g2),
                     transitivity(g2), assortativity_degree(g2)),4)
)
tab

write.table(tab, "tab.txt",sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")


centralidad<-tibble(word = V(g2)$name, eigen = eigen_centrality(g2, scale = T)$vector)
centralidad<-centralidad %>%
  arrange(desc(eigen)) %>%
  head(n = 20)
write.table(centralidad, sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Extract word and cluster information
word <- V(g2)$name
cluster <- kc$membership
eigen = eigen_centrality(g2, scale = T)$vector
# Create dataframe for clusters
cluster <- cbind(word, cluster,eigen)
cluster <- as.data.frame(cluster)


write.table(cluster,"clusters_jep.txt" ,sep = ",", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")



