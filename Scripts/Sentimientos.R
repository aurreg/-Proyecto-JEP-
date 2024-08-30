
################################################################################
# Script JEP CASO 3: Analisis Secuencial de Sentimientos- TALLER 4
# Autor: Cesar Augusto Prieto Sarmiento, Alejandro Urrego Lopez
# Fecha de Creación: 11/03/2024
# Ultima Fecha de Mod: 17/04/2024
# Descripción: Se planea realizar un codigo el cual permita contruir una lista que 
#              Almacene los resultados del analisis secuencial de sentimientos 
#              utilizando el diccionario AFINN video por video, y de esta forma
#              obtener resultados más detallados a nivel gobal de los sentimiento
#              del Caso 03 de la JEP.
# Versión: 1.5
################################################################################

#### ----- CARGA DE PAQUETES UTILIZADOS Y DIRECTORIO DE TRABAJO

library(readr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(purrr)
library(igraph)
library(ngram)
library(xtable)

setwd("C:/Users/HP/Downloads/Taller4")

#### ---- CARGA Y CREACION DE OBJETOS NECESARIOS

stop_words_es <- tibble(word = unlist(
  c(read.table("stopwords-es.txt", quote="\"", comment.char=""))),
  lexicon = "custom")

AFINN <- read_csv("lexico_afinn.csv", col_types = cols(word = col_skip()))
AFINN <- AFINN %>% 
  mutate(sentimiento = ifelse(puntuacion > 0, "Positivo", "Negativo"))

# Definir función para procesar un archivo
procesar_archivo <- function(ruta_archivo) {
  Caso <- read.csv(ruta_archivo, header = FALSE)
  Caso <- c(Caso)
  names(Caso) <- NULL
  Caso <- tibble(line = 1:length(Caso), text = Caso)
  Caso <- Caso %>%
    mutate(text = as.character(text)) %>%
    unnest_tokens(input = text, output = word) %>%
    filter(!is.na(word)) %>%
    filter(!grepl(pattern = '[0-9]', x = word)) %>%
    filter(!word %in% stop_words_es$word)
  
  Caso <- Caso %>%
    filter(word %in% AFINN$palabra) %>%
    left_join(AFINN, by = c("word" = "palabra"), relationship = "many-to-many") %>%
    mutate(sentimiento = ifelse(puntuacion > 0, "Positivo", "Negativo"))
  
  proporciones <- Caso %>%
    count(sentimiento) %>%
    mutate(proporcion = n / sum(n))
  
  # Calcular la media de los valores positivos y negativos
  Medias <- Caso %>%
    mutate(sentimiento = ifelse(puntuacion > 0, "positivo", "negativo")) %>%
    group_by(sentimiento) %>%
    summarise(media = mean(puntuacion, na.rm = TRUE))
  
  # Limpiar objetos que ya no se necesitan
  rm(Caso)
  
  return(list(proporciones = proporciones, Medias = Medias ))
}


# Lista para almacenar las proporciones y medias de cada archivo
resultados_lista <- list()

# Obtener la lista de archivos
archivos <- list.files(path = "Transcripciones", pattern = "\\.txt$", full.names = TRUE)
total_archivos <- length(archivos) # Obtener el total de archivos

# Iterar sobre cada archivo
for (i in 1:total_archivos) {
  archivo <- archivos[i]
  
  # Mostrar barra de progreso
  cat(sprintf("Procesando archivo %d de %d...\n", i, total_archivos))
  
  # Intenta procesar el archivo
  tryCatch({
    resultados <- procesar_archivo(archivo)
    resultados_lista[[archivo]] <- resultados
    rm(resultados) # Limpiar objetos que ya no se necesitan
  }, error = function(e) {
    # Imprimir un mensaje de error si el archivo no se puede leer
    cat("Error al procesar el archivo:", archivo, "\n")
  })
}


#----------------------------------------------------------------------------

#### ---- GUARDAR ARCHIVO RESULTANTE


# Guardar la lista como un archivo .rds
saveRDS(resultados_lista, "Resultados.rds")

# Cargar la lista desde el archivo .rds
Resultados <- readRDS("Resultados.rds")

#----------------------------------------------------------------------------

#### EXTRAER RESULTADOS DE LA LISTA

# Función para extraer Medias Positivas y Negativas juntas
extract_medias <- function(x) {
  if ("positivo" %in% x$Medias$sentimiento & "negativo" %in% x$Medias$sentimiento) {
    MP <- x$Medias %>% filter(sentimiento == "positivo") %>% pull(media)
    MN <- x$Medias %>% filter(sentimiento == "negativo") %>% pull(media)
    return(list(positivo = MP, negativo = MN))
  } else {
    return(NULL)
  }
}

# Aplicar la función a los Resultados
medias_list <- lapply(Resultados, extract_medias)

# Eliminar elementos vacíos
medias_list <- medias_list[!sapply(medias_list, is.null)]

# Extraer MP y MN
MP <- unlist(lapply(medias_list, `[[`, "positivo"))
MN <- unlist(lapply(medias_list, `[[`, "negativo"))


#----------------------------------------------------------------------------------

# Función para extraer Proporciones Positivas y Negativas juntas
extract_proporciones <- function(x) {
  if ("Positivo" %in% x$proporciones$sentimiento & "Negativo" %in% x$proporciones$sentimiento) {
    PP <- x$proporciones %>% filter(sentimiento == "Positivo") %>% pull(proporcion)
    PN <- x$proporciones %>% filter(sentimiento == "Negativo") %>% pull(proporcion)
    return(list(positivo = PP, negativo = PN))
  } else {
    return(NULL)
  }
}

# Aplicar la función a los Resultados
proporciones_list <- lapply(Resultados, extract_proporciones)

# Eliminar elementos vacíos
proporciones_list <- proporciones_list[!sapply(proporciones_list, is.null)]

# Extraer PP y PN
PP <- unlist(lapply(proporciones_list, `[[`, "positivo"))
PN <- unlist(lapply(proporciones_list, `[[`, "negativo"))

#### ----- GRAFICAS

## PARA LA MEDIA

## --------------------------------- MEDIA GRAFICA 1 

par(mfrow = c(1, 2))

# Tracer el histograma y la densidad de las proporciones de sentimientos positivos
hist(MP, main = "Distribución de las medias \n de sentimientos positivos",
     xlab = "Medias de sentimientos positivos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 6), xlim = c(0.8, 2.5 ),
     border = "transparent")
lines(density(MP), col = "4", lwd = 2)
#abline( v = median(MP), col = "black", lwd = 2)

# Tracer el histograma y la densidad de las proporciones de sentimientos negativos
hist((-1)*MN, main = "Distribución de las medias \n de sentimientos negativos",
     xlab = "Medias de sentimientos negativos", ylab = "Frecuencia",
     prob = TRUE, col = "lightgrey", ylim = c(0, 2.5), xlim = c(1.2,3.3 ),
     border = "transparent")
lines(density(-MN), col = "2", lwd = 2)
#abline( v = median(-MN), col = "black", lwd = 2)

# Restaurar el diseño de gráficos predeterminado
par(mfrow = c(1, 1))

## ------------------------------- MEDIA GRAFICA 2 

# Tomar el negativo de MN
MN_negativo <- -MN
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
shapiro_test_MN <- shapiro.test((-1)*MN)
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
wilcoxon_test <- wilcox.test((-1)*MN, MP, alternative = "greater")
Pruebas[1, c("W", "Pvalor")] <- c(wilcoxon_test$statistic, wilcoxon_test$p.value)

# Prueba de suma de rangos con signo de Wilcoxon
wilcoxon_signed_rank_test <- wilcox.test((-1)*MN, MP, alternative = "greater", paired = TRUE)
Pruebas[2, c("W", "Pvalor")] <- c(wilcoxon_signed_rank_test$statistic, wilcoxon_signed_rank_test$p.value)

print(Pruebas)


Tabla <- cbind(Media = c(mean((-1)*MN), mean(MP)), Nediana = c(median((-1)*MN),median(MP)))
row.names(Tabla) <- c("NEG","POS")  
Tabla


## PARA LA PROPORCION


## ------------------------------- PROPORCION GRAFICA 1 

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



############################################################################
# EXTRA - SE PUEDEN INTENTAR OTRAS TECNICAS COMO CLUSTER Y SERIES AL ANALISIS DE SENTIMIENTOS

Medias <- cbind(MP, MN)
row.names(Medias) = NULL
plot(Medias)


Mod <- FactoClass(Medias, dudi.pca, k.clust = 2, scanFC = FALSE)

Mod$cluster
Mod$dudi
Mod$cor.clu
Mod$clus.summ

plot(Medias,Mod$indices)

plotFactoClass(Mod, axislabel = FALSE, Trow = FALSE)

