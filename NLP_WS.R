# Text Analysis Packages
source("NLP_packages.R")

## Maybe you have to set your working directory
setwd("~/Documents/LEHRE_SBG/Workshop_TextMining/RCode/")
## RexEx in R
conversation <- c(
  "User: I am unhappy.
  ELIZA: DO YOU THINK COMING HERE WILL HELP YOU NOT TO BE UNHAPPY
  User: I need some help, that much seems certain.
  ELIZA: WHAT WOULD IT MEAN TO YOU IF YOU GOT SOME HELP
  User: Perhaps I could learn to get along with my mother.
  ELIZA: TELL ME MORE ABOUT YOUR FAMILY
  User: My mother takes care of me.
  ELIZA: WHO ELSE IN YOUR FAMILY TAKES CARE OF YOU
  User: My father.
  ELIZA: YOUR FATHER
  User: You are like my father in some ways."
)

save(conversation, file = "conversation.rda")

## Now we want to use RegEx to split the conversation into a data frame with columns "Speaker" and "Text".
## We can use the strsplit function to split the conversation into lines, and then use gsub to extract the speaker and text from each line.
## We can then use the data.frame function to create a data frame from the resulting list.

text <- strsplit(conversation, "\n")[[1]] # try noquote, tolower
Eliza.df <- data.frame(speaker=rep(NA, length(text)), text=text)

## grep ELIZA and put it in a data frame as speaker
Eliza.df[grep("ELIZA: ",Eliza.df$text), "speaker"] <- "ELIZA"
Eliza.df[grep("User: ",Eliza.df$text), "speaker"] <- "User"

##Now we have to remove ELISA: and User: from text
Eliza.df$text <- gsub("ELIZA: ", "", Eliza.df$text)
Eliza.df$text <- gsub("User: ", "", Eliza.df$text)

## create a Corpus using the qunateda package, which is a package for text analysis (see more on this in NLP_WS2.R)
Eliza.corp <- corpus(Eliza.df, text_field = "text")
summary(Eliza.corp)

##  The grep function
grep("apple",c("crab apple","Apple jack","apple sauce"))
grep("apple",c("crab apple","Apple jack","apple sauce"), value=TRUE)



### gsub and sub functions
text <- "The quick brown fox jumps over the lazy dog."
sub_result <- sub("the", "a", text, ignore.case = TRUE)
gsub_result <- gsub("the", "a", text, ignore.case = TRUE)


################# Examples of Regular Expressions in R #################
# Sample text
text <- c("silverfox", "the fox jumped", "gray", "grey", "123-1234", "Händel", "Haendel", "Handel")

# 1. * - Matches the preceding character zero or more times
grep("fo*x", text) # Matches "fox" and "f" followed by zero or more "o"s.

# 2. + - Matches the preceding character one or more times
grep("fo+x", text) # Matches "fox" but not "f x" (must have at least one "o" after "f").

# 3. ? - Matches the preceding character zero or one time (optional)
grep("gr(a|e)?y", text) # Matches "gray" and "grey".

# 4. . - Matches any character at this position
grep("gr.y", text) # Matches any four-letter word with "gr" at the beginning and "y" at the end.

# 5. $ - Anchors the pattern to the end of the string
grep("fox$", text) # Matches "silverfox" but not "the fox jumped".

# 6. [...] - Matches any character inside the brackets
grepl("gr[ae]y", text) # Matches "gray" or "grey".

# 7. [^...] - Negates the characters inside the brackets
grepl("gr[^a]y", text) # Matches "grey" but not "gray" (excludes "a").

# 8. ^ - Anchors the pattern to the start of the string
grepl("^the", text) # Matches any string that starts with "the".

# 9. {n} - Matches the preceding character exactly n times
grepl("[0-9]{3}-[0-9]{4}", text) # Matches any sequence of "123-1234".

# 10. | - Acts as a logical OR
grepl("gr(a|e)y", text) # Matches "gray" or "grey".

# 11. {m,n} - Matches the preceding character at least m times, but no more than n times
grepl("fo{1,2}x", text) # Matches "fox" or "foox" (one or two "o"s after "f").

# 12. (...) - Groups characters
grepl("H(a|ae)?ndel", text) # Matches "Händel", "Haendel", or "Handel".



### Now we can e.g. grep how many time family, father or mother appears in the ELIZA conversation
grep("family|father|mother", tolower(Eliza.df$text))


