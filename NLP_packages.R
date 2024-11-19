# Text Analysis Packages
library(udpipe)      # For natural language processing (NLP), including tokenization, part-of-speech tagging, and lemmatization.
library(tm)          # Text mining package for text preprocessing (e.g., stopword removal, stemming, creating term-document matrices).
library(tidytext)    # For text mining using the tidyverse, enabling easy integration of textual data with other tidyverse tools.
library(quanteda)    # For advanced quantitative text analysis, such as creating tokens, document-feature matrices, and text statistics.
library(text)        # Tools for working with word embeddings, such as BERT, and text similarity.

# Data Manipulation & Utilities
library(dplyr)       # For data manipulation (e.g., filtering, summarizing, grouping, joining).
library(lubridate)   # Simplifies date and time manipulation (e.g., parsing and formatting dates).
library(tibble)      # Enhances data frames, providing a modern and more consistent framework for tabular data.
library(purrr)       # For functional programming, useful for iterating over lists and applying functions.

# Visualization
library(ggplot2)     # For data visualization using a layered grammar of graphics.
library(wordcloud)   # For generating word clouds from text data.
library(gridExtra)   # For arranging multiple ggplot2 plots in a grid layout.

# Statistical Modeling
library(stm)         # For structural topic modeling, which allows inclusion of document-level metadata.

# File Input and String Manipulation
library(readr)       # For reading and writing data files (e.g., CSVs, text files) efficiently.
library(stringr)     # For string manipulation using consistent and powerful regex patterns.

library(quanteda.textmodels) # For text modeling using the quanteda package.
library(quanteda.textplots)


install_required_packages <- function() {
  packages <- c(
    "udpipe", "tm", "tidytext", "quanteda", "text",
    "dplyr", "lubridate", "tibble", "purrr",
    "ggplot2", "wordcloud", "gridExtra",
    "stm", "readr", "stringr", "quanteda.textmodels", "quanteda.textplots"
  )
  
  # Überprüfen Sie, welche Pakete noch nicht installiert sind
  packages_to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Installieren Sie die fehlenden Pakete
  if (length(packages_to_install) > 0) {
    install.packages(packages_to_install)
  } else {
    message("Alle Pakete sind bereits installiert.")
  }

  # Laden Sie alle Pakete
  lapply(packages, library, character.only = TRUE)
}

# Funktion aufrufen, um die Pakete zu installieren
install_required_packages()