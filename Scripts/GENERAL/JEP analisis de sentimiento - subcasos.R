# Librerias
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

# Leer los subtitulos automatico de la lista del caso 03 de la JEP de youtbe

stop_words_es<-read.table("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/stop_words_spanish.txt", quote="\"", comment.char="")
FilPal <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/FilPal.txt", col_names = FALSE)

stop_words_es<-c(stop_words_es$V1,FilPal$X1)
stop_words_es <- tibble(word = unlist(stop_words_es), lexicon = "custom")


# Leer el archivo línea por línea
correcciones <- readLines("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Grupos de Palabras/palabras_correcciones.txt")

# Separar las palabras originales de sus correcciones
correcciones_df <- as.data.frame(do.call(rbind, strsplit(correcciones, " - ")), stringsAsFactors = FALSE)
names(correcciones_df) <- c("Palabra_Original", "Correccion")
correcciones_df <-correcciones_df%>%
  mutate(Correccion = str_replace(Correccion, "^NA$", ""))


SubCaso_NS <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/NorteSantander_subtitles.csv") #Subtitulos Para Norte de Santander
SubCaso_NS_filtered <- SubCaso_NS[!grepl("Antioquia", SubCaso_NS$Titulo) & !is.na(SubCaso_NS$Subtitulos), ]
#SubCaso_CC <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/CostaCaribe_subtitles.csv") #Subtitulos Para Costa Caribe
#SubCaso_C <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Casanare_subtitles.csv") # Subtitulos para Casanare
#SubCaso_M <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Meta_subtitles.csv") #Subtitulos Para Meta
#SubCaso_H <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Huila_subtitles.csv") #Subtitulos Para  Huila
#SubCaso_A <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/Antioquia_subtitles.csv") #Subtitulos Para Antioquia


SubCaso<-SubCaso_NS_filtered

Palabra<-'Norte de santander'
# Norte de santander
# Costa Caribe
# Casanare
# Meta
# Huila
# Antioquia



setwd(paste0("C:/Users/Pc/Desktop/-Proyecto-JEP-/Outputs/Analisis-Sentimientos/",Palabra))

subtitulos_JEP_03 <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Scraping/subtitulos_JEP_03.csv")


head(SubCaso$Titulo,20)

subtitulos_JEP_03 <- subtitulos_JEP_03 %>%
  filter(!(Titulo %in% SubCaso$Titulo) & !is.na(Subtitulos) ) %>%  # Filtrar los títulos que NO están en SubCaso
  filter(str_detect(Titulo, Palabra)) %>%  # # Filtrar aquellos que contienen la palabra
  #filter(str_detect(Titulo, Palabra)|str_detect(Titulo, 'Catatumbo')) %>% # only for NS
  as.data.frame()


# se crea un dataframe con titulos que puedan tener palabras clave 
subtitulos_JEP_03<-rbind(subtitulos_JEP_03,SubCaso[,-1])%>%
  as.data.frame()%>%
  mutate(Subtitulos= str_replace_all(Subtitulos, "\\[Música\\]", ""))  %>%
  filter(!is.na(Subtitulos) )
# se elimina [Música] dado que no son subtitulos sino musica del video
# Preprocesar los subtítulos
subtitulos_JEP_03 <- within(subtitulos_JEP_03, {
  Subtitulos <- str_to_lower(Subtitulos)  # Convertir a minúsculas
  Subtitulos <- str_replace_all(Subtitulos, "[^a-záéíóúüñ ]", "")  # Eliminar caracteres no alfabéticos
  Subtitulos <- str_squish(Subtitulos)  # Eliminar espacios en blanco adicionales
})



subtitulos_JEP_03$Titulo




length(subtitulos_JEP_03$Titulo)

# Preprocesar los subtítulos
# Tokenizar todos los subtítulos


tokenized_subtitles <- subtitulos_JEP_03 %>%
  unnest_tokens(input = Subtitulos, output = word) %>%
  filter(!is.na(word))

# Eliminar stopwords

tokenized_subtitles <- tokenized_subtitles %>%
  anti_join(stop_words_es, by = "word")

#udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez

# Cargar el modelo de idioma español
model = udpipe_load_model(file = "C:/Users/Pc/Desktop/-Proyecto-JEP-/Scripts/spanish-gsd-ud-2.5-191206.udpipe")

# Anotar todos los subtítulos utilizando udpipe
# esto se puede hacer por video pero tarda mucho

tidy_subtitles_annotated = udpipe_annotate(model, x = tokenized_subtitles$word, 
                                           doc_id = tokenized_subtitles$Titulo)

tidy_subtitles_annotated = as_tibble(tidy_subtitles_annotated)

# Renombrar la columna para tener el token
names(tidy_subtitles_annotated)[6] = "Token"


# Continuar con el procesamiento de los tokens
tidy_subtitles_annotated <- tidy_subtitles_annotated %>%
  # Verifica que 'Token' exista antes de eliminar puntuación
  mutate(Token = str_remove_all(Token, "[[:punct:]]")) %>%
  
  # Unir con el data frame de correcciones usando un left_join
  left_join(correcciones_df, by = c("lemma" = "Palabra_Original")) %>%
  
  # Reemplazar lemma con la corrección si existe
  mutate(lemma = case_when(
    !is.na(Correccion) ~ Correccion,  # Reemplaza lemma con la corrección si existe
    TRUE ~ lemma                      # Mantiene el lemma original si no hay corrección
  )) %>%
  
  # Eliminar la palabra "NA" literal en lemma
  mutate(lemma = str_replace(lemma, "^NA$", "")) %>%
  
  # Reemplazar Token con lemma si está disponible
  mutate(Token = case_when(
    !is.na(lemma) ~ lemma,
    TRUE ~ Token
  )) %>%
  
  # Filtrar Tokens vacíos
  filter(Token != "") %>%
  
  # Quitar acentos y caracteres especiales en la columna Token
  #mutate(Token = iconv(Token, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  
  # Eliminar cualquier carácter que no sea alfabético o numérico
  mutate(Token = str_remove_all(Token, "[^[:alnum:]]")) %>%
  
  # Convertir todo a minúsculas
  mutate(Token = str_to_lower(Token))

tidy_subtitles_annotated<-tidy_subtitles_annotated %>%
  mutate(upos = case_when(
    lemma == 'compareciente' ~ 'NOUN',  # Condición para cambiar 'upos' si 'lemma' es 'compareciente'
    lemma == 'santander'~ 'NOUN',
    TRUE ~ upos  # Mantiene el valor actual de 'upos' si no se cumple la condición anterior
  ))


# Analisis frecuencia lemmas
frec<-tidy_subtitles_annotated %>%
  filter(Token!='él',Token!='yo',Token!='tú')%>%
count(Token, sort = TRUE) %>%
  head(n = 15)

frec<-as.numeric(frec[15,2])

color_inicial <- "#008080"
color_final <- "#011f4b"

# Casanare n > 5000
# huila n > 10000

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

ggsave("frec.png",
       plot = p1, 
       width = 12, height = 8, units = "cm", dpi = 300, bg = "white")

## NUBE DE PALABRAS

Col = c("#14213d")


png("word_cloud.png",
   width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")
# Definir el tamaño del layout con la función png()
set.seed(12)
tidy_subtitles_annotated %>%  filter(Token!='él',Token!='yo',Token!='tú')%>%
  count(Token, sort = TRUE) %>%
  with(wordcloud(words = Token, scale=c(3,.4), freq = n, max.words = 100, colors= Col ))
dev.off()
# Análisis de Frecuencia por Etiqueta POS

frecuencia<-tidy_subtitles_annotated%>%
  group_by(Token)%>%
  summarise(frecuencia=n())

frecuencia<-frecuencia%>%
  arrange(desc(frecuencia))

tidy_subtitles_annotated %>% 
  count(upos) %>% 
  ggplot() +
  geom_col(aes(x = reorder(upos, n), y = n, fill = upos)) +
  labs(x = "Etiqueta POS", y = "Frecuencia", title = "Subtítulos: Video JEP") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))

# Análisis de los 5 lemas más frecuentes por categoría gramatical (NOUN, PROPN, VERB, ADJ)
png("noun.png",
   width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")
tidy_subtitles_annotated %>% 
  filter(upos %in% c('NOUN')) %>% 
  count(lemma) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  labs(x = "Lemma", y = "Frecuencia", title = "") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))

dev.off()


png("verb.png", 
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")

tidy_subtitles_annotated %>% 
  filter(upos %in% c('VERB')) %>% 
  count(lemma) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ggplot() +
  geom_col(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  labs(x = "Lemma", y = "Frecuencia", title = "") +
  coord_flip() +
  theme(legend.position = "none", text = element_text(size = 18))

dev.off()

# Analisis de sentimiento 
# Importacion diccionario AFINN
lexico_afinn <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/lexico_afinn.csv", 
                         col_types = cols(word = col_skip()))

# como es una traduccion hay palabras repetidas por lo tanto se hace un promedio de los puntajes
lexico_afinn<-lexico_afinn %>%group_by(palabra)%>%summarise(puntuacion=mean(puntuacion))

# Importacion de diccionario con palabras negativas y positivas

positive_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/positive_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Positivo")
negative_words <- read_csv("C:/Users/Pc/Desktop/-Proyecto-JEP-/Data/negative_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")

sentiment_words <- bind_rows(positive_words, negative_words)

# hacer join con los lemmas obtenidos por el lematizador de palabras

tidy_subtitles_annotated2<-tidy_subtitles_annotated%>%left_join(lexico_afinn,by=c('Token'='palabra'))
str(tidy_subtitles_annotated2)

# dejar las columnas de interes para hacer el analisis de sentimiento 

AFIN<-tidy_subtitles_annotated2%>%select(doc_id,Token,puntuacion)

# hacer el join con el df de palabras negativas y positivas
AFIN<-AFIN%>%left_join(sentiment_words, by=c('Token'='word'))

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
  filter(sentiment=='Positivo')%>%as.data.frame()


  
#right_join(AFIN_negativo,by=c('doc_id'='doc_id'))%>%
  #select(sentiment=sentiment.x, media=media.x,frecuencia=frecuencia.x)



# Crear un histograma de sentimientos positivos
png("hist_score.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")

hist(AFIN_positivo$media, main = "",
     xlab = "Mean Sentiment Score", ylab = "Frequency",
     prob = TRUE, col = rgb(0.2, 0.6, 0.8, 0.5), ylim = c(0, 4.5+.5), xlim = c(0.7, 2.4+.5),
     border = "transparent")

# Añadir la densidad de los sentimientos positivos
lines(density(AFIN_positivo$media), col = "blue", lwd = 2)

# Añadir el histograma de sentimientos negativos sobre el mismo gráfico
hist((-1)*AFIN_negativo$media, prob = TRUE, col = rgb(1, 0.2, 0.2, 0.5),
     border = "transparent", add = TRUE)

# Añadir la densidad de los sentimientos negativos
lines(density((-1)*AFIN_negativo$media), col = "red", lwd = 2)

# Añadir leyenda para identificar los sentimientos positivos y negativos
legend("topright", legend = c("Positive", "Negative"), 
      col = c("blue", "red"), lwd = 2, lty = 1, bty = "n", cex = 1)

dev.off()

#Tomar el negativo de MN
MN_negativo <- (-1)*AFIN_negativo$media
MP <- AFIN_positivo$media

 #MN_negativo<-append(MN_negativo, 0, after = 1) #correr para antioquia
names(MN_negativo) <- NULL

# Crear un data frame para ggplot
data <- data.frame(x = 1:length(MP), MP = MP, MN = MN_negativo)

png("serie_score.png",
    width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")

ggplot(data, aes(x = x)) +
  geom_line(aes(y = MN, color = "Negativo"), lwd = 0.5) +
  geom_line(aes(y = MP, color = "Positivo"), lwd = 0.5) +
  labs(x = "Index", y = "Mean Sentiment Score", title = "") +
  scale_color_manual(name = "Sentiment", labels = c("Negative", "Positive"),
                     values = c("Negativo" = "2", "Positivo" = "4")) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))


dev.off()

#### PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA
# PRUEBA DE NORMALIDAD PARA MP
shapiro_test_MP <- shapiro.test(MP)

# PRUEBA DE NORMALIDAD PARA MN
shapiro_test_MN <- shapiro.test(MN_negativo)

# Pruebas de Diferencia de Medias y Medianas

# Prueba de Mann-Whitney-Wilcoxon
wilcoxon_test <- wilcox.test(MN_negativo, MP, alternative = "greater",paired = T)

# Cargar la librería car para la prueba de Levene
# install.packages("car") # Si no tienes la librería
library(car)

# Prueba de Levene para igualdad de varianzas
levene_test <- leveneTest(c(MP, MN_negativo) ~ factor(c(rep(1, length(MP)), rep(2, length(MN_negativo)))))

# Prueba t de Student (para igualdad de medias)
t_test <- t.test(MN_negativo, MP, alternative = "greater", var.equal = TRUE,paired = T)
t_test2 <- t.test(MN_negativo, MP, alternative = "greater", var.equal = F,paired = T)

# Calcular medias y medianas
media_MP <- mean(MP)
media_MN <- mean(MN_negativo)
mediana_MP <- median(MP)
mediana_MN <- median(MN_negativo)

# Crear un data frame para almacenar todos los resultados
resultados_totales <- data.frame(
  Estadística = c("Media MP", "Media MN", 
                  "Mediana MP", "Mediana MN",
                  "Shapiro-Wilk MP P-valor", "Shapiro-Wilk MN P-valor",
                  "Mann-Whitney W", "Mann-Whitney P-valor", 
                  "Prueba Levene F", "Prueba Levene P-valor",
                  "Prueba t T var.equa", "Prueba t P-valor var.equa",
                  "Prueba t T", "Prueba t P-valor"),
  Valor = c(media_MP, media_MN, 
            mediana_MP, mediana_MN,
            shapiro_test_MP$p.value, shapiro_test_MN$p.value,
            wilcoxon_test$statistic, wilcoxon_test$p.value, 
            levene_test$"F value"[1], levene_test$"Pr(>F)"[1],
            t_test$statistic, t_test$p.value,
            t_test2$statistic, t_test2$p.value)
)

# Redondear los valores en el data frame
resultados_totales$Valor <- round(resultados_totales$Valor, 3)

# Imprimir el resultado en una tabla con encabezado

# Crear el archivo donde se guardarán todos los resultados
archivo_resultados <- file("resultados_estadisticos.txt", "w")

# Guardar el primer conjunto de resultados en el archivo
cat("Resumen de pruebas estadísticas (MP vs MN_negativo):\n", file = archivo_resultados)
write.table(resultados_totales, file = archivo_resultados, row.names = FALSE, col.names = TRUE, sep = "\t", append = TRUE)

# Agregar un separador entre los resultados
cat("\n--------------------------\n", file = archivo_resultados)



## PARA LA PROPORCION ----

## PROPORCION GRAFICA 1 
PP <- AFIN_positivo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)
PN <- AFIN_negativo$frecuencia/(AFIN_positivo$frecuencia+AFIN_negativo$frecuencia)

# Crear densidades kernel para PP y PN
densidad_PP <- density(PP)
densidad_PN <- density(PN)

# Graficar la densidad de Kernel de PP con color azul
png("dens_proportion.png",
   width = 12, height = 8, units = "cm", res = 300, pointsize = 8, bg = "white")
plot(densidad_PN, main = "",
     xlab = "Proportion", ylab = "Density",
     col = rgb(1, 0.2, 0.2, 0.5), lwd = 2)

# Add vertical lines at the mean of PP and PN
abline(v = mean(PP), col = "blue", lwd = 2, lty = 2)
abline(v = mean(PN), col = "red", lwd = 2, lty = 2)

# Add legend
legend("topleft", legend = c("Positive", "Negative"),
       col = c("blue", "red"), lwd = 2, lty = 2,
       bty = "n", cex = 1)

dev.off()

## PRUEBAS DE BONDAD DE AJUSTE

# PRUEBAS DE DIFERENCIA DE PROPORCIONES Y VARIANZAS
### PRUEBAS DE BONDA DE AJUSTE PARA LA MEDIA
# PRUEBA DE NORMALIDAD PARA PP
shapiro_test_PP <- shapiro.test(PP)

# PRUEBA DE NORMALIDAD PARA PN
shapiro_test_PN <- shapiro.test(PN)

# Pruebas de Diferencia de Medias y Medianas

# Prueba de Mann-Whitney-Wilcoxon
wilcoxon_test <- wilcox.test(PN, PP, alternative = "greater",paired = T)

# Cargar la librería car para la prueba de Levene
# install.packages("car") # Si no tienes la librería
library(car)

# Prueba de Levene para igualdad de varianzas
levene_test <- leveneTest(c(PP, PN) ~ factor(c(rep(1, length(PP)), rep(2, length(PN)))))

# Prueba t de Student (para igualdad de medias)
t_test <- t.test(PN, PP, alternative = "greater", var.equal = TRUE,paired = T)
t_test2 <- t.test(PN, PP, alternative = "greater", var.equal = F,paired = T)

# Calcular medias y medianas
media_PP <- mean(PP)
media_PN <- mean(PN)
mediana_PP <- median(PP)
mediana_PN <- median(PN)

# Crear un data frame para almacenar todos los resultados
resultados_totales <- data.frame(
  Estadística = c("Media PP", "Media PN", 
                  "Mediana PP", "Mediana PN",
                  "Shapiro-Wilk PP P-valor", "Shapiro-Wilk PN P-valor",
                  "Mann-Whitney W", "Mann-Whitney P-valor", 
                  "Prueba Levene F", "Prueba Levene P-valor",
                  "Prueba t T var.equa", "Prueba t P-valor var.equa",
                  "Prueba t T", "Prueba t P-valor"),
  Valor = c(media_PP, media_PN, 
            mediana_PP, mediana_PN,
            shapiro_test_PP$p.value, shapiro_test_PN$p.value,
            wilcoxon_test$statistic, wilcoxon_test$p.value, 
            levene_test$"F value"[1], levene_test$"Pr(>F)"[1],
            t_test$statistic, t_test$p.value,
            t_test2$statistic, t_test2$p.value)
)

# Redondear los valores en el data frame
resultados_totales$Valor <- round(resultados_totales$Valor, 3)


# Imprimir el resultado en una tabla con encabezado
cat("Resultados de la comparación de proporciones (PP vs PN):\n", file = archivo_resultados)
write.table(resultados_totales, file = archivo_resultados, row.names = FALSE, col.names = TRUE, sep = "\t", append = TRUE)

# Cerrar el archivo para guardar los cambios
close(archivo_resultados)

