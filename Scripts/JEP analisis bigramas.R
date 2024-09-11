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
setwd("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data")

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

subtitulos= paste(subtitulos_JEP_03$Subtitulos, collapse = ' ')

subtitulos <- tibble(text = subtitulos)

subtitulos <- subtitulos %>%
  unnest_tokens(input = text, output = word, token = "regex", pattern = "\\s+")



# Extract tokens
tokens <- subtitulos$word

# Create bigrams from tokens and avoid multiple edges
bigrams <- sapply(1:(length(tokens) - 1), function(i) {
  words <- sort(c(tokens[i], tokens[i + 1]))
  paste(words, collapse = " ")
})

# Convert bigrams to a vector
bigrams <- as.vector(bigrams)

# Convert bigrams to dataframe
bigrams<- str_split_fixed(bigrams, " ", 2)

word_1 <- bigrams[, 1]
word_2 <- bigrams[, 2]

bigrams2 <- cbind(word_1, word_2)
bigrams2 <- as.data.frame(bigrams2)

bigrams2<-bigrams2%>%
  filter(!word_1 %in% stop_words_es$word & !word_2 %in% stop_words_es$word)%>%
  filter(word_1 != word_2)%>%
  count(word_1, word_2, sort = TRUE) %>%
  rename(weight = n)

# Calculate skewness for different thresholds
threshold <- unique(bigrams2$weight)
count <- bigrams2$weight
library(EnvStats)
s <- NULL
for (i in 1:length(threshold)) { 
  s[i] <- skewness(bigrams2[count > threshold[i], ]$weight)
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

g <- bigrams2%>%
  filter(weight > 40) %>%
  select(word_1,word_2)%>%
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
  Fast_Greedy = mod_fast_greedy,
  Infomap = mod_infomap,
  Label_Propagation = mod_label_prop,
  #Leading_Eigen = mod_leading_eigen,
  Leiden = mod_leiden,
  Louvain = mod_louvain,
  Spinglass = mod_spinglass,
  Walktrap = mod_walktrap
)

# Finding the method with the highest modularity
best_method <- names(which.max(modularity_values))http://127.0.0.1:39927/graphics/plot_zoom_png?width=1536&height=814
best_modularity <- max(modularity_values)

# Printing the method with the highest modularity
cat("Based on the modularity values, the method with the highest modularity is", best_method, "\n")
cat("Modularity value:", best_modularity, "\n")

# Display unique memberships of Louvain algorithm
unique(kc_louvain$membership)

# Summarize the graph
summary(g2)

suppressMessages(suppressWarnings(library(RColorBrewer)))

cols <- c(brewer.pal(9,"Set1")[1:9],brewer.pal(8,"Set2")[1:7],brewer.pal(8,"Set2")[1:7],brewer.pal(12,"Set3")[1:3])
# viz 1
set.seed(123)
plot(g2, layout = layout_with_kk, vertex.color = adjustcolor(cols[kc_louvain$membership], 0.1), 
     vertex.frame.color =adjustcolor(cols[kc_louvain$membership],0.5), vertex.size = ifelse(1.2*degree(g2)>2,1.2*degree(g2),2), 
     vertex.label = NA, edge.color=adjustcolor('gray',0.8))



# Extract word and cluster information
word <- V(g2)$name
cluster <- kc_louvain$membership

# Create dataframe for clusters
cluster <- cbind(word, cluster)
cluster <- as.data.frame(cluster)



# Exporting for analysis with Python and GPT
write.csv(cluster, "clusters_jep.csv", row.names = FALSE, fileEncoding = "UTF-8")

