## THE STM MODEL
## source the previous file to have the whole data
source("NLP_packages.R")

source("NLP_WS2.R")



## Now we want to model the speeches of Trump and Clinton using structural topic models

# Konvertieren der DTM in ein Format, das von stm verwendet werden kann
dfm_stm <- as.dfm(dtm_sparse)

# Erstellen der Metadaten
meta <- TC.Corpus %>%
  filter(speech_id %in% as.numeric(rownames(dtm_sparse))) %>%
  select(speech_id, speaker)

# Estimate  STM-Model: no prevalence no content
stm_model <- stm(documents = dfm_stm, 
                 K = 10,  # Anzahl der Themen 
                 data = meta, 
                 max.em.its = 200, 
                 verbose = TRUE)




##Schätzen des STM-Modells mit Prävalenz und thematischem Inhalt
stm_model_content <- stm(documents = dfm_stm,
                 K = 10,  # Anzahl der Themen 
                 content = ~ speaker, 
                 data = meta, 
                 max.em.its = 200, 
                 verbose = TRUE)



# Schätzen der Effekte des Sprechers auf die Themenproportionen
effect <- estimateEffect(1:10 ~ speaker, stm_model, meta = meta, uncertainty = "Global")


# Plotten der Ergebnisse
plot.estimateEffect(effect, covariate = "speaker", 
                    model = stm_model, 
                    method = "difference", 
                    cov.value1 = "Trump", cov.value2 = "Clinton",
                    xlab = "Difference in Topic Proportions (Trump - Clinton)",
                    main = "Differences in Topic Proportions between Trump and Clinton")

summary(effect)


plot.STM(stm_model_content, type = "perspectives", 
         covariate = "speaker", 
         topics = 1:1,  # Anzahl der Themen
         main = "Word Distribution: Clinton vs. Trump")
##################################################################
## Interpret the results: see what happens
labelTopics(stm_model)
 ## footnote: 
#  FREX (Frequency-Relative Exclusivity) and LIFT are two metrics used to identify important words or terms in text analysis.

# FREX combines term frequency and exclusivity, aiming to find terms that occur frequently but are exclusive to a subset of documents, highlighting significant terms for specific contexts.
# LIFT measures the strength of association between two terms in a given dataset. It compares the co-occurrence of two terms to what would be expected if they occurred independently. A LIFT score greater than 1 indicates a stronger association than expected by chance.


## plot the results
plot.STM(stm_model, type = "summary", main = "STM Model")

## I want to comapre the theat values between TRUMP and CLINTON
## I will use the function estimateEffect to estimate the effect of the speaker on the topic proportions    
## I will use the function plot.estimateEffect to plot the results
## I will use the function plot.STM to plot the results

effect <- estimateEffect(1:10 ~ speaker, stm_model, meta = meta, uncertainty = "Global")

# Plotten der Ergebnisse
par(mfrow=c(1,1))
plot.estimateEffect(effect, covariate = "speaker", 
                    model = stm_model, 
                    method = "difference", 
                    cov.value1 = "Trump", cov.value2 = "Clinton",
                    xlab = "Difference in Topic Proportions (Trump - Clinton)",
                    main = "Differences in Topic Proportions between Trump and Clinton")

# Anzeigen der geschätzten Effekte
summary(effect)




### and a wordfish model for the positions of each speech
# Konvertieren der DTM in ein quanteda dfm
dfm_quanteda <- convert(dtm_sparse, to = "dfm")

# Erstellen der Metadaten
meta <- TC.Corpus %>%
  filter(speech_id %in% as.numeric(rownames(dtm_sparse))) %>%
  select(speech_id, speaker)

# Schätzen des Wordfish-Modells
wordfish_model <- textmodel_wordfish(dfm_quanteda, dir = c(1, 118))

# Anzeigen der Ergebnisse
summary(wordfish_model)

## plot it 
textplot_scale1d(wordfish_model)
textplot_scale1d(wordfish_model, groups = meta$speaker)
