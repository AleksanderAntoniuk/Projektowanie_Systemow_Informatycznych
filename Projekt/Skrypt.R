# Skrypt do analizy sentymentu wypowiedzi posłów (z lematyzacją)
# Porównuje okres opozycji i okres rządu na podstawie dwóch plików .txt
#
# Wymagania:
# 1) R (wersja ≥ 4.0)
# 2) Pakiety: tidyverse, tidytext, udpipe, stopwords, stringr
# 3) Plik modelu UDPipe dla języka polskiego (np. "polish-ud-2.5-191206.udpipe") 
#    – pobierz z https://github.com/jwijffels/udpipe.models.ud (wersja dla polskiego)
# 4) Leksykon polskiego sentymentu SentiLex-PL w formacie CSV (kolumny: word, polarity)
#    – umieść plik "SentiLex-PL.csv" w katalogu roboczym
#
# W określonym miejscu podaj nazwy obu plików .txt:
# - file_opozitia    ← teksty z okresu opozycji
# - file_rzad        ← teksty z okresu rządu
#
# Każdy plik .txt powinien zawierać w każdej linii jedną wypowiedź posła.

# 1. Załaduj wymagane pakiety ------------------------------------------------
library(tidyverse)    # dplyr, readr, tibble, ggplot2 itp.
library(tidytext)     # unnest_tokens()
library(udpipe)       # lematyzacja UDPipe
library(stopwords)    # lista polskich stop-słów
library(stringr)      # str_remove_all()

# 2. Wczytaj model UDPipe dla języka polskiego --------------------------------
model_path <- "polish-ud-2.5-191206.udpipe"
if (!file.exists(model_path)) {
  stop("Brakuje pliku modelu UDPipe: ", model_path,
       "\nPobierz go z: https://github.com/jwijffels/udpipe.models.ud")
}
ud_model <- udpipe_load_model(model_path)

# 3. Wczytaj leksykon SentiLex-PL ----------------------------------------------
lex_path <- "SentiLex-PL.csv"
if (!file.exists(lex_path)) {
  stop("Brakuje pliku SentiLex-PL.csv w katalogu roboczym.")
}
sentilex <- read_csv(lex_path, 
                     col_types = cols(
                       word     = col_character(),
                       polarity = col_double()
                     )) %>%
  rename(term = word, score = polarity) %>%
  filter(!is.na(term), !is.na(score))


# 4. Funkcja: lematyzacja i obliczenie sentymentu dla pliku .txt ---------------
process_speech <- function(file_path, ud_model, lexicon, stop_list) {
  # 4.1 Wczytaj plik .txt (każda linia to jedna wypowiedź)
  if (!file.exists(file_path)) {
    stop("Nie znaleziono pliku: ", file_path)
  }
  lines <- read_lines(file_path, locale = locale(encoding = "UTF-8"))
  df_raw <- tibble(text = lines) %>%
    mutate(doc_id = as.character(row_number()))
  
  # 4.2 Usuń treść w nawiasach okrągłych (np. uwagi ze stenogramu)
  df_raw <- df_raw %>%
    mutate(
      text = str_remove_all(text, "\\s*\\([^)]*\\)")
    )
  
  # 4.3 Lematyzacja za pomocą UDPipe
  message("Lematyzacja: ", file_path, " ...")
  ud_annot <- udpipe_annotate(ud_model, x = df_raw$text, doc_id = df_raw$doc_id)
  ud_df    <- as_tibble(ud_annot)
  
  # 4.4 Przygotuj listę lematów per dokument
  ud_lemmas <- ud_df %>%
    filter(!upos %in% c("PUNCT", "SYM", "NUM")) %>%
    mutate(lemma = tolower(lemma)) %>%
    group_by(doc_id) %>%
    summarize(text_lemmas = paste(lemma, collapse = " ")) %>%
    ungroup()
  
  # 4.5 Połącz lematy z oryginalnym df
  df <- df_raw %>%
    left_join(ud_lemmas, by = "doc_id")
  
  # 4.6 Tokenizacja lematyzowanych tekstów i usuwanie stop-słów
  df_tokens <- df %>%
    select(doc_id, text_lemmas) %>%
    unnest_tokens(token, text_lemmas) %>%
    filter(!token %in% stop_list) %>%
    filter(str_detect(token, "^[[:alpha:]]+$"))
  
  # 4.7 Dopasowanie tokenów do leksykonu i sumaryczny sentyment
  df_sent <- df_tokens %>%
    inner_join(lexicon, by = c("token" = "term")) %>%
    group_by(doc_id) %>%
    summarize(sentiment = sum(score, na.rm = TRUE)) %>%
    ungroup()
  
  # 4.8 Połączenie wyników i uzupełnienie NA = 0
  df <- df %>%
    left_join(df_sent, by = "doc_id") %>%
    mutate(sentiment = if_else(is.na(sentiment), 0, sentiment))
  
  return(df)
}

# 5. Wskaż nazwy plików .txt z wypowiedziami -----------------------------------
#    Podaj ścieżki lub nazwy plików:
file_opozycja <- "wypowiedzi_opozcja.txt"
file_rzad     <- "wypowiedzi_rzad.txt"

# 6. Analiza okresu opozycji ----------------------------------------------------
message("\n===================== OPOZYCJA =====================")
df_opoz <- process_speech(file_opozycja, ud_model, sentilex, stop_pl)

# Podstawowe statystyki dla okresu opozycji
mean_opoz <- mean(df_opoz$sentiment, na.rm = TRUE)
cat("Średni sentyment (opozycja): ", round(mean_opoz, 3), "\n")
cat("Liczba wypowiedzi (opozycja): ", nrow(df_opoz), "\n\n")

# 7. Analiza okresu rządu --------------------------------------------------------
message("\n======================= RZĄD ========================")
df_rzad <- process_speech(file_rzad, ud_model, sentilex, stop_pl)

# Podstawowe statystyki dla okresu rządu
mean_rzad <- mean(df_rzad$sentiment, na.rm = TRUE)
cat("Średni sentyment (rząd):    ", round(mean_rzad, 3), "\n")
cat("Liczba wypowiedzi (rząd):    ", nrow(df_rzad), "\n\n")

# 8. Porównanie rozkładów i wizualizacje (opcjonalnie) --------------------------
# Połącz dane i dodaj etykietę okresu
df_opoz   <- df_opoz %>%   mutate(okres = "opozycja")
df_rzad   <- df_rzad %>%   mutate(okres = "rzad")
df_all    <- bind_rows(df_opoz, df_rzad)

# 8a. Histogram rozkładu sentymentu
p_hist <- ggplot(df_all, aes(x = sentiment, fill = okres)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~ okres, scales = "free_y") +
  labs(title = "Rozkład sentymentu (opozycja vs rząd)", 
       x = "Sentyment (suma punktów)", 
       y = "Liczba wypowiedzi") +
  theme_minimal()

# 8b. Boxplot porównujący oba okresy
p_box <- ggplot(df_all, aes(x = okres, y = sentiment, fill = okres)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(title = "Boxplot sentymentu: opozycja vs rząd", 
       x = "Okres", 
       y = "Sentyment (suma punktów)") +
  theme_minimal() +
  theme(legend.position = "none")

# 8c. Zapisz wykresy do plików PNG (opcjonalnie)
ggsave(filename = "histogram_sentiment.png", plot = p_hist, width = 8, height = 4)
ggsave(filename = "boxplot_sentiment.png",  plot = p_box,  width = 5, height = 4)

# 9. Zapisz szczegółowe wyniki do CSV (opcjonalnie) ----------------------------
write_csv(df_opoz, "wyniki_opozycja_sentiment.csv")
write_csv(df_rzad, "wyniki_rzad_sentiment.csv")

message("Analiza zakończona!\n",
        "- Wygenerowano pliki CSV: wyniki_opozycja_sentiment.csv, wyniki_rzad_sentiment.csv\n",
        "- Wygenerowano wykresy: histogram_sentiment.png, boxplot_sentiment.png\n")
