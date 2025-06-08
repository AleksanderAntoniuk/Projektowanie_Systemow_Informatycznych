# Skrypt do analizy sentymentu wypowiedzi posłów (z lematyzacją)
# Korzysta z listy pozytywnych/negatywnych wyrazów z Kaggle zamiast SentiLex-PL

# 1. Załaduj wymagane pakiety ------------------------------------------------
library(tidyverse)    # dplyr, readr, tibble, ggplot2 itp.
library(tidytext)     # unnest_tokens()
library(udpipe)       # lematyzacja UDPipe
library(stopwords)    # lista polskich stop-słów
library(stringr)      # str_remove_all() i inne
library(tm)

# 2. Wczytaj (lub pobierz) model UDPipe dla języka polskiego -------------------
model_path <- "polish-pdb-ud-2.5-191206.udpipe"
if (!file.exists(model_path)) {
  message("Nie znaleziono lokalnego pliku modelu UDPipe")
  message("Model UDPipe pobrany jako: ", model_path)
}
ud_model <- udpipe_load_model(model_path)

# 3. Wczytaj własną listę stop-słów TODO -----------------------------

stopwords <- readLines("polish_stopwords.txt")

# 4. Zbuduj słownik sentymentu z plików z Kaggle ------------------------------
pos_path <- "positive_words_pl.txt"
neg_path <- "negative_words_pl.txt"

if (!file.exists(pos_path) || !file.exists(neg_path)) {
  stop("Brak plików 'polish-positive-words.txt' lub 'polish-negative-words.txt' w katalogu roboczym.")
}

pos <- read_lines(pos_path, locale = locale(encoding = "UTF-8")) %>%
  discard(~ str_starts(.x, ";")) %>%
  str_trim() %>%
  discard(~ .x == "")

neg <- read_lines(neg_path, locale = locale(encoding = "UTF-8")) %>%
  discard(~ str_starts(.x, ";")) %>%
  str_trim() %>%
  discard(~ .x == "")

sentilex <- tibble(
  term  = c(pos, neg),
  score = c(rep( 1, length(pos)),
            rep(-1, length(neg)))
) %>%
  distinct(term, .keep_all = TRUE) %>%
  filter(!is.na(term))

# 5. Funkcja: lematyzacja i obliczenie sentymentu dla pliku .txt ---------------
process_speech <- function(file_path, ud_model, lexicon) {
  # 5.1 Wczytaj plik .txt (każda linia to jedna wypowiedź)
  if (!file.exists(file_path)) {
    stop("Nie znaleziono pliku: ", file_path)
  }
  lines <- read_lines(file_path, locale = locale(encoding = "UTF-8"))
  df_raw <- tibble(text = lines) %>%
    mutate(doc_id = as.character(row_number()))
  
  # 5.2 Usuń treść w nawiasach okrągłych (np. uwagi ze stenogramu)
  df_raw <- df_raw %>%
    mutate(
      text = str_remove_all(text, "\\s*\\([^)]*\\)")
    )
  
  # 5.3 Lematyzacja za pomocą UDPipe
  message("Lematyzacja: ", file_path, " …")
  ud_annot <- udpipe_annotate(ud_model, x = df_raw$text, doc_id = df_raw$doc_id)
  ud_df    <- as_tibble(ud_annot)
  
  # 5.4 Przygotuj listę lematów per dokument
  ud_lemmas <- ud_df %>%
    filter(!upos %in% c("PUNCT", "SYM", "NUM")) %>%
    mutate(lemma = tolower(lemma)) %>%
    group_by(doc_id) %>%
    summarize(text_lemmas = paste(lemma, collapse = " ")) %>%
    ungroup()
  
  # 5.5 Połącz lematy z oryginalnym df
  df <- df_raw %>%
    left_join(ud_lemmas, by = "doc_id")
  
  # 5.6 Tokenizacja lematyzowanych tekstów i usuwanie stop-słów
  df_tokens <- df %>%
    select(doc_id, text_lemmas) %>%
    unnest_tokens(token, text_lemmas) %>%
    filter(str_detect(token, "^[[:alpha:]]+$"))
  
  # 5.7 Dopasowanie tokenów do leksykonu i sumaryczny sentyment
  df_sent <- df_tokens %>%
    inner_join(lexicon, by = c("token" = "term")) %>%
    group_by(doc_id) %>%
    summarize(sentiment = sum(score, na.rm = TRUE)) %>%
    ungroup()
  
  # 5.8 Połączenie wyników i uzupełnienie NA = 0
  df <- df %>%
    left_join(df_sent, by = "doc_id") %>%
    mutate(sentiment = if_else(is.na(sentiment), 0, sentiment))
  
  return(df)
}

# 6. Wskaż nazwy plików .txt z wypowiedziami -----------------------------------
file_opozycja_fogiel <- "Fogiel 24,25.txt"
file_rzad_fogiel     <- "Fogiel 22,23.txt"

file_opozycja_nowicka <- "Nowicka 2021.txt"
file_rzad_nowicka <- "Nowicka 2023.txt"

file_opozycja_tusk <- "Donald_Tusk_2022-2023.txt"
file_rzad_tusk <- "Donald_Tusk_2024-2025.txt"

#ANALIZA - FOGIEL
# 7. Analiza okresu opozycji ----------------------------------------------------
message("\n===================== OPOZYCJA =====================")
df_opoz <- process_speech(file_opozycja_fogiel, ud_model, sentilex)

# Podstawowe statystyki dla okresu opozycji
mean_opoz <- mean(df_opoz$sentiment, na.rm = TRUE)
cat("Średni sentyment (opozycja): ", round(mean_opoz, 3), "\n")
cat("Liczba wypowiedzi (opozycja): ", nrow(df_opoz), "\n\n")

# 8. Analiza okresu rządu ---------------------------------------------------------
message("\n======================= RZĄD ========================")
df_rzad <- process_speech(file_rzad_fogiel, ud_model, sentilex)

# Podstawowe statystyki dla okresu rządu
mean_rzad <- mean(df_rzad$sentiment, na.rm = TRUE)
cat("Średni sentyment (rząd):    ", round(mean_rzad, 3), "\n")
cat("Liczba wypowiedzi (rząd):    ", nrow(df_rzad), "\n\n")

# 9. Porównanie rozkładów i wizualizacje (opcjonalnie) --------------------------
df_opoz <- df_opoz %>% mutate(okres = "opozycja")
df_rzad <- df_rzad %>% mutate(okres = "rzad")
df_all  <- bind_rows(df_opoz, df_rzad)

# 9a. Histogram rozkładu sentymentu
ggplot(df_all, aes(x = sentiment, fill = okres)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~ okres, scales = "free_y") +
  labs(title = "Rozkład sentymentu dla wypowiedzi R. Fogla (opozycja vs rząd)", 
       x = "Sentyment (suma punktów)", 
       y = "Liczba wypowiedzi") +
  theme_minimal()

# 9b. Boxplot porównujący oba okresy
ggplot(df_all, aes(x = okres, y = sentiment, fill = okres)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.) +
      labs(title = "Boxplot sentymentu dla wypowiedzi R. Fogla: opozycja vs rząd", 
           x = "Badany okres", 
           y = "Sentyment (suma punktów)") +
          theme_minimal() +
          theme(legend.position = "none")




#ANALIZA - NOWICKA
# 7. Analiza okresu opozycji ----------------------------------------------------
message("\n===================== OPOZYCJA =====================")
df_opoz <- process_speech(file_opozycja_nowicka, ud_model, sentilex)

# Podstawowe statystyki dla okresu opozycji
mean_opoz <- mean(df_opoz$sentiment, na.rm = TRUE)
cat("Średni sentyment (opozycja): ", round(mean_opoz, 3), "\n")
cat("Liczba wypowiedzi (opozycja): ", nrow(df_opoz), "\n\n")

# 8. Analiza okresu rządu ---------------------------------------------------------
message("\n======================= RZĄD ========================")
df_rzad <- process_speech(file_rzad_nowicka, ud_model, sentilex)

# Podstawowe statystyki dla okresu rządu
mean_rzad <- mean(df_rzad$sentiment, na.rm = TRUE)
cat("Średni sentyment (rząd):    ", round(mean_rzad, 3), "\n")
cat("Liczba wypowiedzi (rząd):    ", nrow(df_rzad), "\n\n")

# 9. Porównanie rozkładów i wizualizacje (opcjonalnie) --------------------------
df_opoz <- df_opoz %>% mutate(okres = "opozycja")
df_rzad <- df_rzad %>% mutate(okres = "rzad")
df_all  <- bind_rows(df_opoz, df_rzad)

# 9a. Histogram rozkładu sentymentu
ggplot(df_all, aes(x = sentiment, fill = okres)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~ okres, scales = "free_y") +
  labs(title = "Rozkład sentymentu dla wypowiedzi W. Nowickiej (opozycja vs rząd)", 
       x = "Sentyment (suma punktów)", 
       y = "Liczba wypowiedzi") +
  theme_minimal()

# 9b. Boxplot porównujący oba okresy
ggplot(df_all, aes(x = okres, y = sentiment, fill = okres)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.) +
  labs(title = "Boxplot sentymentu dla wypowiedzi W. Nowickiej: opozycja vs rząd", 
       x = "Badany okres", 
       y = "Sentyment (suma punktów)") +
  theme_minimal() +
  theme(legend.position = "none")



#ANALIZA - TUSK
# 7. Analiza okresu opozycji ----------------------------------------------------
message("\n===================== OPOZYCJA =====================")
df_opoz <- process_speech(file_opozycja_tusk, ud_model, sentilex)

# Podstawowe statystyki dla okresu opozycji
mean_opoz <- mean(df_opoz$sentiment, na.rm = TRUE)
cat("Średni sentyment (opozycja): ", round(mean_opoz, 3), "\n")
cat("Liczba wypowiedzi (opozycja): ", nrow(df_opoz), "\n\n")

# 8. Analiza okresu rządu ---------------------------------------------------------
message("\n======================= RZĄD ========================")
df_rzad <- process_speech(file_rzad_tusk, ud_model, sentilex)

# Podstawowe statystyki dla okresu rządu
mean_rzad <- mean(df_rzad$sentiment, na.rm = TRUE)
cat("Średni sentyment (rząd):    ", round(mean_rzad, 3), "\n")
cat("Liczba wypowiedzi (rząd):    ", nrow(df_rzad), "\n\n")

# 9. Porównanie rozkładów i wizualizacje (opcjonalnie) --------------------------
df_opoz <- df_opoz %>% mutate(okres = "opozycja")
df_rzad <- df_rzad %>% mutate(okres = "rzad")
df_all  <- bind_rows(df_opoz, df_rzad)

# 9a. Histogram rozkładu sentymentu
ggplot(df_all, aes(x = sentiment, fill = okres)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~ okres, scales = "free_y") +
  labs(title = "Rozkład sentymentu dla wypowiedzi D. Tuska (opozycja vs rząd)", 
       x = "Sentyment (suma punktów)", 
       y = "Liczba wypowiedzi") +
  theme_minimal()

# 9b. Boxplot porównujący oba okresy
ggplot(df_all, aes(x = okres, y = sentiment, fill = okres)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.) +
  labs(title = "Boxplot sentymentu dla wypowiedzi D. Tuska: opozycja vs rząd", 
       x = "Badany okres", 
       y = "Sentyment (suma punktów)") +
  theme_minimal() +
  theme(legend.position = "none")

