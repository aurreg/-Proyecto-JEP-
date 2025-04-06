# Libraries
# lemmatizer udpipe
library(udpipe)     # For lemmatization and POS tagging
library(stringr)     # For string manipulation
library(tibble)      # For creating tibbles (data frames)
library(readr)       # For reading CSV files
library(ggplot2)     # For plotting
library(wordcloud)   # For creating word clouds
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(tidyverse)   # For a collection of useful packages
library(tidytext)    # For text mining
library(magrittr)    # For pipe operator (%>%)
library(igraph)      # For network analysis (though not used here)
library(ngram)       # For n-gram analysis (though not used here)
library(xtable)      # For creating LaTeX tables 

# Read automatic subtitles from the JEP case 03 YouTube list

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"", comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)
stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")


# Read the file line by line
correcciones <- readLines("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Grupos de Palabras/palabras_correcciones.txt")

# Separate the original words from their corrections
correcciones_df <- as.data.frame(do.call(rbind, strsplit(correcciones, " - ")), stringsAsFactors = FALSE)
names(correcciones_df) <- c("Palabra_Original", "Correccion")
correcciones_df <-correcciones_df%>%
  mutate(Correccion = str_replace(Correccion, "^NA$", ""))


SubCaso_NS <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/NorteSantander_subtitles.csv") #Subtitles for Norte de Santander
SubCaso_NS_filtered <- SubCaso_NS[!grepl("Antioquia", SubCaso_NS$Titulo) & !is.na(SubCaso_NS$Subtitulos), ]
SubCaso_CC <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/CostaCaribe_subtitles.csv") #Subtitles for Costa Caribe
SubCaso_C <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Casanare_subtitles.csv") # Subtitles for Casanare
SubCaso_M <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Meta_subtitles.csv") #Subtitles for Meta
SubCaso_H <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Huila_subtitles.csv") #Subtitles for Huila
SubCaso_A <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Antioquia_subtitles.csv") #Subtitles for Antioquia


SubCaso<-SubCaso_A

Palabra<-'Antioquia'
# Norte de santander
# Costa Caribe
# Casanare
# Meta
# Huila
# Antioquia



setwd(paste0("C:/Users/Pc/Desktop/-Proyecto-JEP-/Outputs/Sentiment Analysis/",Palabra))

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")


head(SubCaso$Titulo,20)

subtitulos_JEP_03 <- subtitulos_JEP_03 %>%
  filter(!(Titulo %in% SubCaso$Titulo) & !is.na(Subtitulos) ) %>%  # Filter titles that are NOT in SubCaso and are not NA
  filter(str_detect(Titulo, Palabra)) %>%   # Filter those that contain the word
  #filter(str_detect(Titulo, Palabra)|str_detect(Titulo, 'Catatumbo')) %>% # only for NS
  as.data.frame()


# Create a dataframe with titles that may have keywords  
subtitulos_JEP_03<-rbind(subtitulos_JEP_03,SubCaso[,-1])%>%
  as.data.frame()%>%
  mutate(Subtitulos= str_replace_all(Subtitulos, "\\[Música\\]", ""))   %>%
  filter(!is.na(Subtitulos) )
# remove [Música] since it is not subtitles but music from the video
# Preprocess the subtitles
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)   # Convert to lowercase
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")   # Remove non-alphabetic characters
  Subtitulos <- str_squish(Subtitulos)   # Remove extra whitespace
})


subtitulos_JEP_03$Titulo




length(subtitulos_JEP_03$Titulo)

# Preprocess the subtitles
# Tokenize all subtitles
tokenized_subtitles <- subtitulos_JEP_03 %>%
  unnest_tokens(input = Subtitulos, output = word) %>%
  filter(!is.na(word))

# Remove stopwords
tokenized_subtitles <- tokenized_subtitles %>%
  anti_join(stop_words_es, by = "word")

#udpipe::udpipe_download_model('spanish') # Uncomment when running for the first time

# Load the Spanish language model
model <- udpipe_load_model(file = "C:/Users/Pc/Desktop/-Proyecto-JEP-/Scripts/spanish-gsd-ud-2.5-191206.udpipe")

# Annotate all subtitles using udpipe
# this can be done by video but takes a long time
tidy_subtitles_annotated <- udpipe_annotate(model,
                                            x = tokenized_subtitles$word,
                                            doc_id = tokenized_subtitles$Titulo
)

tidy_subtitles_annotated <- as_tibble(tidy_subtitles_annotated)

# Rename the column to have the token
names(tidy_subtitles_annotated)[6] <- "Token"

# Read the file line by line
correcciones <- readLines("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Grupos de Palabras/palabras_correcciones.txt")

# Separate the original words from their corrections
correcciones_df <- as.data.frame(do.call(rbind, strsplit(correcciones, " - ")), stringsAsFactors = FALSE)
names(correcciones_df) <- c("Palabra_Original", "Correccion")
correcciones_df <-correcciones_df%>%
  mutate(Correccion = str_replace(Correccion, "^NA$", ""))

# Continue processing the tokens
tidy_subtitles_annotated <- tidy_subtitles_annotated %>%
  # Verify that 'Token' exists before removing punctuation
  mutate(Token = str_remove_all(Token, "[[:punct:]]")) %>%
  
  # Join with the corrections data frame using a left_join
  left_join(correcciones_df, by = c("lemma" = "Palabra_Original")) %>%
  
  # Replace lemma with the correction if it exists
  mutate(lemma = case_when(
    !is.na(Correccion) ~ Correccion, # Replace lemma with the correction if it exists
    TRUE ~ lemma # Keep the original lemma if there is no correction
  )) %>%
  
  # Remove the literal "NA" word in lemma
  mutate(lemma = str_replace(lemma, "^NA$", "")) %>%
  
  # Replace Token with lemma if available
  mutate(Token = case_when(
    !is.na(lemma) ~ lemma,
    TRUE ~ Token
  )) %>%
  
  # Filter empty Tokens
  filter(Token != "") %>%
  
  # Remove accents and special characters in the Token column
  #mutate(Token = iconv(Token, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  
  # Remove any non-alphanumeric characters
  mutate(Token = str_remove_all(Token, "[^[:alnum:]]")) %>%
  
  # Convert everything to lowercase
  mutate(Token = str_to_lower(Token))

tidy_subtitles_annotated <- tidy_subtitles_annotated %>%
  mutate(upos = case_when(
    lemma == 'compareciente' ~ 'NOUN', # Condition to change 'upos' if 'lemma' is 'compareciente'
    lemma == 'santander' ~ 'NOUN',
    TRUE ~ upos # Keeps the current value of 'upos' if the previous condition is not met
  ))

frec<-tidy_subtitles_annotated %>%
  filter(Token!='él',Token!='yo',Token!='tú')%>%
  count(Token, sort = TRUE) %>%
  head(n = 15)

frec<-as.numeric(frec[15,2])

color_inicial <- "#008080"
color_final <- "#011f4b"



p1 <- tidy_subtitles_annotated   %>%  filter(Token!='él',Token!='yo',Token!='tú')%>%
  count(Token, sort = TRUE) %>%
  filter(n >=     frec) %>%
  mutate(Token = reorder(Token, n)) %>%
  ggplot(aes(x = Token, y = n, fill = n)) +  # Usar 'n' para definir el degradado
  theme_light() + 
  geom_col(alpha = 1) +  # No es necesario especificar fill aquí
  scale_fill_gradient(low = color_inicial, high = color_final) +  # Agregar el degradado de color
  xlab(NULL) +
  ylab("Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

p1

p1

ggsave("frec.png",
       plot = p1,
       width = 12, height = 8, units = "cm", dpi = 300, bg = "white"
)

## WORD CLOUD

Col <- c("#14213d")
# Define the layout size with the png() function

png("word_cloud.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")

set.seed(12)
tidy_subtitles_annotated %>%  filter(Token!='él',Token!='yo',Token!='tú')%>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(3,.4), freq = n, max.words = 100, colors= Col ))
dev.off()

# Frequency Analysis by POS Tag

frecuencia <- tidy_subtitles_annotated %>%
  group_by(Token) %>%
  summarise(frecuencia = n())

frecuencia <- frecuencia %>%
  arrange(desc(frecuencia))

tidy_subtitles_annotated %>%
  count(upos) %>%
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  labs(x = "POS Tag", y = "Frequency", title = "Subtitles: JEP Video") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))

# Analysis of the 5 most frequent lemmas by grammatical category (NOUN, PROPN, VERB, ADJ)
png("noun.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white"
)
tidy_subtitles_annotated %>%
  filter(upos %in% c('NOUN')) %>%
  count(lemma) %>%
  slice_max(order_by = n, n = 5) %>%
  ggplot() +
  geom_col(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  labs(x = "Lemma", y = "Frequency", title = "") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))
dev.off()

png("verb.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white"
)

tidy_subtitles_annotated %>%
  filter(upos %in% c('VERB')) %>%
  count(lemma) %>%
  slice_max(order_by = n, n = 5) %>%
  ggplot() +
  geom_col(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  labs(x = "Lemma", y = "Frequency", title = "") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))
dev.off()

# Sentiment analysis
# Import AFINN dictionary
# Sentiment analysis
# Import AFINN dictionary
lexico_afinn <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/lexico_afinn.csv",
                         col_types = cols(word = col_skip())
)

# Since it is a translation, there are repeated words, so an average of the scores is made
lexico_afinn <- lexico_afinn %>% group_by(palabra) %>% summarise(puntuacion = mean(puntuacion))

# Import dictionary with negative and positive words

positive_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/positive_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Positivo")
negative_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/negative_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")

sentiment_words <- bind_rows(positive_words, negative_words)

# Join with the lemmas obtained by the word lemmatizer

tidy_subtitles_annotated2 <- tidy_subtitles_annotated %>% left_join(lexico_afinn, by = c('Token' = 'palabra'))
str(tidy_subtitles_annotated2)

# Leave the columns of interest to do the sentiment analysis

AFIN <- tidy_subtitles_annotated2 %>% select(doc_id, Token, puntuacion)

# Join with the df of negative and positive words
AFIN <- AFIN %>% left_join(sentiment_words, by = c('Token' = 'word'))

# Leave only the rows that do not have NA in AFINN (puntuacion) or sentiment
# With the filter, new values are added to sentiment
# If the score is greater than 0 and there is an NA, it is a positive word
# If it is less, it is negative
# If it is equal, it is neutral
# Then, if there is no NA in sentiment, the categorization that existed at the beginning is left

AFIN <- AFIN %>%
  filter(!is.na(puntuacion) | !is.na(sentiment)) %>%
  mutate(
    sentiment = case_when(
      puntuacion > 0 & is.na(sentiment) ~ "Positivo",
      puntuacion < 0 & is.na(sentiment) ~ "Negativo",
      puntuacion == 0 & is.na(sentiment) ~ "Neutro",
      TRUE ~ sentiment
    )
  )

# The number of negative and positive words is counted
# As well as the average of the scores

AFIN2 <- AFIN %>%
  group_by(doc_id, sentiment) %>%
  summarise(media = mean(puntuacion, na.rm = T), frecuencia = n())

AFIN2 %>% filter(sentiment == 'Neutro')

AFIN_negativo <- AFIN2 %>% filter(sentiment == 'Negativo' & !is.na(media))

AFIN_positivo <- AFIN2 %>%
  filter(!is.na(media)) %>%
  filter(sentiment == 'Positivo') %>%
  as.data.frame()

AFIN_completo <- AFIN_negativo %>%
  full_join(AFIN_positivo, by = "doc_id", suffix = c("_negativo", "_positivo")) %>%
  replace_na(list(
    media_negativo = 0, media_positivo = 0,
    frecuencia_negativo = 0, frecuencia_positivo = 0
  ))

#right_join(AFIN_negativo,by=c('doc_id'='doc_id'))%>%
#select(sentiment=sentiment.x, media=media.x,frecuencia=frecuencia.x)

# Create a histogram of positive sentiments
png("hist_score.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white"
)

plot(density((-1) * AFIN_completo$media_negativo),
     main = "",
     xlab = "Score", ylab = "Density",
     col = "red", lwd = 2,
     xlim=c(min(c(density((-1) * AFIN_completo$media_negativo)$x,density( AFIN_completo$media_positivo)$x)),
            max(c(density((-1) * AFIN_completo$media_negativo)$x,density( AFIN_completo$media_positivo)$x))
))

# Add the density of positive sentiments

# Add the histogram of negative sentiments on the same graph

# Add the density of negative sentiments
lines(density( AFIN_completo$media_positivo), col = "blue", lwd = 2)

# Add legend to identify positive and negative sentiments
legend(
  "topleft",
  legend = c("Positive", "Negative"),
  col = c("blue", "red"), lwd = 2, lty = 1, bty = "n", cex = 1
)

dev.off()

# Take the negative of MN
MN_negativo <- (-1) * AFIN_completo$media_negativo
MP <- AFIN_completo$media_positivo

#MN_negativo<-append(MN_negativo, 0, after = 1) #run for antioquia
names(MN_negativo) <- NULL

# Create a data frame for ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

png("serie_score.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white"
)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  labs(x = "Index", y = "Mean Sentiment Score", title = "") +
  scale_color_manual(
    name = "Sentiment", labels = c("Negative", "Positive"),
    values = c("Negativo" = "2", "Positivo" = "4")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

#### GOODNESS OF FIT TESTS FOR THE MEAN
shapiro_test <- shapiro.test(MP - MN_negativo)

# TESTS FOR DIFFERENCE OF MEANS AND MEDIANS

# Mann-Whitney-Wilcoxon test
wilcoxon_test <- wilcox.test(MN_negativo, MP, alternative = "greater", paired = T)



# Student's t-test (for equality of means)
t_test <- t.test(MN_negativo, MP, alternative = "greater", var.equal = TRUE, paired = T)

# Calculate means and medians
media_MP <- mean(MP)
media_MN <- mean(MN_negativo)
mediana_MP <- median(MP)
mediana_MN <- median(MN_negativo)

# Create a data frame to store all results
resultados_totales <- data.frame(
  Estadística = c(
    "Mean Positive Score", "Mean Negative Score",
    "Median Positive Score", "Median Negative Score", "Shapiro-Wilk Difference P-value",
    "Mann-Whitney P-value",
    
    "t-test P-value"
    
  ),
  Valor = c(
    media_MP, media_MN,
    mediana_MP, mediana_MN,
    shapiro_test$p.value,
    wilcoxon_test$p.value,
    t_test$p.value
  )
)

# Round the values in the data frame
resultados_totales$Valor <- round(resultados_totales$Valor, 3)

# Print the result in a table with header

# Create the file where all the results will be saved
archivo_resultados <- file("results.txt", "w")

# Save the first set of results in the file
cat("summary score results:\n", file = archivo_resultados)
write.table(resultados_totales, file = archivo_resultados, row.names = FALSE, col.names = TRUE, sep = "\t", append = TRUE)

# Add a separator between the results
cat("\n--------------------------\n", file = archivo_resultados)

## FOR PROPORTION ----

## GRAPHIC PROPORTION 1
PP <- AFIN_completo$frecuencia_positivo / (AFIN_completo$frecuencia_positivo + AFIN_completo$frecuencia_negativo)
PN <- AFIN_completo$frecuencia_negativo / (AFIN_completo$frecuencia_positivo + AFIN_completo$frecuencia_negativo)

# Create kernel densities for PP and PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Plot the Kernel density of PP in blue
png("dens_proportion.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white"
)
plot(densidad_PN,
     main = "",
     xlab = "Proportion", ylab = "Density",
     col = rgb(1, 0.2, 0.2, 0.5), lwd = 2
)

# Add vertical lines at the mean of PP and PN
abline(v = mean(PP), col = "blue", lwd = 2, lty = 2)
abline(v = mean(PN), col = "red", lwd = 2, lty = 2)

# Add legend
legend(
  "topleft",
  legend = c("Positive", "Negative"),
  col = c("blue", "red"), lwd = 2, lty = 2,
  bty = "n", cex = 1
)

dev.off()

## GOODNESS OF FIT TESTS

# PROPORTION AND VARIANCE DIFFERENCE TESTS
### GOODNESS OF FIT TESTS FOR THE MEAN
shapiro_test <- shapiro.test(PP - PN)


# Mann-Whitney-Wilcoxon test
wilcoxon_test <- wilcox.test(PN, PP, alternative = "greater", paired = T)


t_test <- t.test(PN, PP, alternative = "greater", var.equal = TRUE, paired = T)

# Calculate means and medians
media_PP <- mean(PP)
media_PN <- mean(PN)
mediana_PP <- median(PP)
mediana_PN <- median(PN)

# Create a data frame to store all results
resultados_totales <-data.frame(
  Estadística = c(
    "Mean Positive Proportion", "Mean Negative Proportion",
    "Median Positive Proportion", "Median Negative Proportion", "Shapiro-Wilk Difference P-value",
    "Mann-Whitney P-value",
    
    "t-test P-value"
    
  ),
  Valor = c(
    media_PP, media_PN ,
    mediana_PP, mediana_PN ,
    shapiro_test$p.value,
    wilcoxon_test$p.value,
    t_test$p.value
  )
)

# Round the values in the data frame
resultados_totales$Valor <- round(resultados_totales$Valor, 3)

# Print the result in a table with header
cat("summary proportions results:\n", file = archivo_resultados)
write.table(resultados_totales, file = archivo_resultados, row.names = FALSE, col.names = TRUE, sep = "\t", append = TRUE)

# Close the file to save the changes
close(archivo_resultados)
