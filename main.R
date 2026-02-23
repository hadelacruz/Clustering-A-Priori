# Copia del dataset original
library(dplyr)
library(tidyr)
library(stringr)
library(fastDummies)
install.packages("fastDummies")


movies <- read.csv("movies_2026.csv", header = TRUE)



movies_clean <- movies

# =====================================================
# 1️⃣ Eliminar columnas que no aportan al clustering
# =====================================================

movies_clean <- movies_clean %>%
  select(-id,
         -originalTitle,
         -title,
         -homePage,
         -actorsCharacter,
         -releaseDate,
         -productionCompany,
         -productionCompanyCountry,
         -actors,
         -actorsPopularity,
         -productionCountry,
         -director,
         -originalLanguage,
         -video)

# =====================================================
# 2️⃣ Crear variables agregadas de actorsPopularity
# =====================================================

# Primero recuperar actorsPopularity desde el dataset original
actor_pop_list <- strsplit(movies$actorsPopularity, "\\|")

mean_actor_popularity <- sapply(actor_pop_list, function(x) {
  vals <- as.numeric(x)
  vals <- vals[!is.na(vals)]
  if(length(vals) == 0) return(NA)
  mean(vals)
})

max_actor_popularity <- sapply(actor_pop_list, function(x) {
  vals <- as.numeric(x)
  vals <- vals[!is.na(vals)]
  if(length(vals) == 0) return(NA)
  max(vals)
})

movies_clean$mean_actor_popularity <- mean_actor_popularity
movies_clean$max_actor_popularity  <- max_actor_popularity

# =====================================================
# 3️⃣ Transformación logarítmica de variables financieras
# =====================================================

movies_clean <- movies_clean %>%
  mutate(
    budget_log  = log1p(budget),
    revenue_log = log1p(revenue)
  ) %>%
  select(-budget, -revenue)

# =====================================================
# 4️⃣ Crear variables dummy para los géneros más frecuentes
# =====================================================

# Separar géneros en formato largo
genres_long <- movies %>%
  select(genres) %>%
  separate_rows(genres, sep = "\\|")

# Obtener Top 8 géneros más frecuentes
top_genres <- genres_long %>%
  count(genres, sort = TRUE) %>%
  slice(1:8) %>%
  pull(genres)

# Crear columna con solo el primer género relevante encontrado
movies_clean$main_genre <- sapply(strsplit(movies$genres, "\\|"), function(x) {
  g <- x[x %in% top_genres]
  if(length(g) == 0) return("Other")
  g[1]
})

# Crear dummies
movies_clean <- dummy_cols(movies_clean,
                           select_columns = "main_genre",
                           remove_first_dummy = FALSE,
                           remove_selected_columns = TRUE)

# =====================================================
# 5️⃣ Eliminar columna original de géneros
# =====================================================

movies_clean <- movies_clean %>%
  select(-genres)

# =====================================================
# 6️⃣ Eliminar NA
# =====================================================

movies_clean <- na.omit(movies_clean)

# =====================================================
# 7️⃣ Escalamiento (muy importante)
# =====================================================

movies_scaled <- scale(movies_clean)

# Convertir a data frame
movies_scaled <- as.data.frame(movies_scaled)

# =====================================================
# Resultado final
# =====================================================

dim(movies_scaled)
summary(movies_scaled)
colnames(movies_clean)

movies_clean <- movies_clean %>%
  filter(voteCount > 0)

movies_clean <- movies_clean %>%
  filter(voteAvg  > 0)

movies_clean <- movies_clean %>%
  filter(budget_log > 0)

movies_clean <- movies_clean %>%
  filter(revenue_log > 0)

str(movies_clean)
