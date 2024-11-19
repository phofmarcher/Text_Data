source("NLP_packages.R")


setwd("~/Documents/LEHRE_SBG/Workshop_TextMining/RCode/")
###############################################################################
### TRUMP VS CLINTON
# Verzeichnis mit den Textdateien
text_files_trump <- "Clinton-TrumpCorpus/Trump"
text_files_clinton <- "Clinton-TrumpCorpus/Clinton"
# Liste der Textdateien im Verzeichnis
trump_files <- list.files(text_files_trump, pattern = "\\.txt$", full.names = TRUE)
clinton_files <- list.files(text_files_clinton, pattern = "\\.txt$", full.names = TRUE)



# Funktion zum Lesen der Textdateien
read_text_file <- function(file) {
  content <- read_file(file)
  # Extract title
  title <- str_extract(content, "(?<=<title=)[^>]+")
  title <- str_remove(title, "\"") # remove \" from title"
  # Extrahieren date of speech
  date <- str_extract(content, "(?<=<date:)[^>]+")
  date <- ymd(date)
  # Entfernen der Metainformationen aus dem Inhalt
  text <- str_remove_all(content, "<title=[^>]+>|<date:[^>]+>")

  tibble(title = title, date = date, text = text)
}


## footnote:
## Maybe some notes on this: 
# The expression (?<=<title=)[^>]+ is a regular expression (regex) used to extract text from a string. Here's a breakdown:

    # (?<=<title=):
# This is a positive lookbehind, which matches text that is preceded by <title= but does not include <title= in the match.
# Example: In <title="My Title">, it looks for the text after <title=.
    # [^>]+:
# This matches one or more characters (+) that are not > ([^>] is a negated character set, meaning "not >").
# Example: In <title="My Title">, it matches My Title" (up to but not including >).




# Lesen aller Textdateien und Erstellen eines DataFrames and add colum with speaker. map_df is a function from the purrr package that applies a function to each element of a list and then combines the results into a single data frame.
Trump.C <- trump_files %>%
  map_df(read_text_file)  %>% mutate(speaker = "Trump")

Clinton.C <- clinton_files %>%
        map_df(read_text_file) %>% mutate(speaker = "Clinton")



## We can detect how gets more Applause by greping <APPLAUSE> in the text for Trump and Clinton
Trump.C <- Trump.C %>%
  mutate(laughter_count = str_count(text, "<LAUGHTER>"),
         applause_count = str_count(text, "<APPLAUSE>"))

Clinton.C <- Clinton.C %>%
  mutate(laughter_count = str_count(text, "<LAUGHTER>"),
         applause_count = str_count(text, "<APPLAUSE>"))

# Erstellen eines DataFrames mit allen Texten
TC.Corpus <- rbind(Trump.C, Clinton.C)


## hmm maybe we should first remove the applause and laughter
TC.Corpus$text <- gsub("<APPLAUSE>", "", TC.Corpus$text)
TC.Corpus$text <- gsub("<LAUGHTER>", "", TC.Corpus$text)


## We can now plot the number of applause and laughter for Trump and Clinton
ggplot(TC.Corpus, aes(x = speaker, y = applause_count)) +
  geom_boxplot() +
  labs(title = "Number of Applause by Speaker",
       x = "Speaker",
       y = "Number of Applause")



## footnote: we  can try this :-)
############################################################################
###  Preprocessing Text Data
## 1.) POS, we will use the udpipe package
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Funktion zum Anwenden von POS-Tagging auf den Text
pos_tagging <- function(text) {
  annotation <- udpipe_annotate(ud_model, x = text, parser="none")
  as.data.frame(annotation)
}

TC.Corpus$pos_tags <- lapply(TC.Corpus$text, pos_tagging)
## Now check what the pos_tags look like. We can check e.g.for first document
table(TC.Corpus$pos_tags[[1]]$upos)


## differences in POS between Trump and Clinton
# Function to create POS-Tables for clinton and Trump
pos_table <- function(pos_tags) {
  prop.table(table(pos_tags$upos))
}
Trump_pos_tables <- lapply(TC.Corpus %>% filter(speaker == "Trump") %>% pull(pos_tags), pos_table)
Clinton_pos_tables <- lapply(TC.Corpus %>% filter(speaker == "Clinton") %>% pull(pos_tags), pos_table)

average_pos_tags <- function(pos_tables) {
  pos_df <- do.call(rbind, lapply(pos_tables, as.data.frame))
  pos_df %>%
    group_by(Var1) %>%
    summarise(avg_count = mean(Freq))
}
Trump_avg_pos_tags <- average_pos_tags(Trump_pos_tables)
Clinton_avg_pos_tags <- average_pos_tags(Clinton_pos_tables)

pos_diff <- merge(Trump_avg_pos_tags, Clinton_avg_pos_tags, by = "Var1", suffixes = c("_Trump", "_Clinton"))
pos_diff <- pos_diff %>%
  mutate(diff = avg_count_Trump - avg_count_Clinton)


######################################################################
## add speech id
TC.Corpus <- TC.Corpus %>%
  mutate(speech_id = row_number())

# preprocess text
preprocess_text <- function(text) {
  text <- tolower(text)  # Konvertieren in Kleinbuchstaben
  text <- removePunctuation(text)  # Entfernen der Satzzeichen
  text <- removeNumbers(text)  # Entfernen der Zahlen
  text <- removeWords(text, stopwords("en"))  # Entfernen der Stoppwörter
  text <- stripWhitespace(text)  # Entfernen von überflüssigen Leerzeichen
  return(text)
}

## footnote:
## If you wan to do this with quanteda you can use the tokens function
# tokens_clean <- tokens(TC.Corpus$text, 
#                        remove_punct = TRUE,  # Remove punctuation
#                        remove_symbols = TRUE,  # Remove symbols
#                        remove_numbers = TRUE)  # Remove numbers



## what are the stopwords in the tm package
head(stopwords("en"))

## are there still any stopwords in TC.Corpus$text
TC.Corpus$text <- sapply(TC.Corpus$text, removeWords, stopwords("en"))

# Anwenden der Vorverarbeitung auf den Text in TC.Corpus
TC.Corpus2 <- TC.Corpus ## copy TC.Corpus
TC.Corpus$text <- sapply(TC.Corpus$text, preprocess_text)




# Funktion zum Erstellen von Unigrammen und Bigrammen
create_ngrams <- function(text, n) {
  unlist(lapply(ngrams(words(text), n), paste, collapse = " "))
}

# Create DataFrames with Unigram and Bigram
# unigrams <- TC.Corpus %>%
#   unnest_tokens(word, text, token = "words")

bigrams <- TC.Corpus %>%
  unnest_tokens(output=bigram, input=text, token = "ngrams", n =2)


# Combine Unigram and Bigram
#all_ngrams <- bind_rows(unigrams, bigrams)

# create DTM
dtm <- bigrams %>%
  count(speech_id, bigram) %>%
  cast_dtm(speech_id, bigram, n)

## remove sparse terms, more than 99% or 90%
dtm_sparse <- removeSparseTerms(dtm, 0.9)


## Now we can use the dtm to create a word cloud for Trump and Clinton
trump_docs <- which(TC.Corpus$speech_id %in% rownames(dtm_sparse) & TC.Corpus$speaker == "Trump")
clinton_docs <- which(TC.Corpus$speech_id %in% rownames(dtm_sparse) & TC.Corpus$speaker == "Clinton")

wordcloud_trump <- function() {
  term_matrix <- as.matrix(dtm_sparse[trump_docs, ])
  term_freq <- colSums(term_matrix)
  wordcloud(names(term_freq), term_freq, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

wordcloud_clinton <- function() {
  term_matrix <- as.matrix(dtm_sparse[clinton_docs, ])
  term_freq <- colSums(term_matrix)
  wordcloud(names(term_freq), term_freq, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

par(mfrow = c(1, 2))
wordcloud_trump()
title("Trump Wordcloud")
wordcloud_clinton()
title("Clinton Wordcloud")

## what do we see, e.g. applause appears quite often, so maybe remove it
ind <- grep("applause", colnames(dtm_sparse))
dtm_sparse <- dtm_sparse[, -ind]
## and create wordlcouds again.... 


