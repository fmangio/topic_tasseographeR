


# =============================================================================
# Topic tasseographeR — Example 2 (Aspect Based Sentiment Analysis)
# Replication script: LDA (K = 40) + export inputs for Topic tasseographeR
# Produces:
#   - td_beta.csv (topic–word distribution) for Topic Content app
#   - ds.csv      (document–topic distribution + Text) for Topic Function app
#   - top words plots, top documents tables, and session info
#
# How to run (from repo root in RStudio):
#   install.packages("renv"); renv::restore()
#   source("examples/case2_aspect_based_sa.R")
# =============================================================================


library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

#data: https://myleott.com/op-spam.html


# read in our data
reviews <- read_csv("deceptive-opinion.csv")

#focus on truthful only

reviews <- reviews %>% filter(deceptive=="truthful")
#create document identifier

reviews<- reviews%>%
  mutate(Document = row_number())


# create a document term matrix to clean
reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("hotel", "room", "chicago", "stayed", "stay"))


# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% 
  mutate(term = gsub("[[:punct:]]", "", term)) %>%  # Remove punctuation
  mutate(term = gsub("[[:digit:]]", "", term)) %>%  # Remove numbers
  anti_join(stop_words, by = c("term" = "word")) %>%  # Remove English stopwords
  anti_join(custom_stop_words, by = c("term" = "word"))  # Remove custom stopwords

# stem the words (e.g. convert each word to its stem, where applicable)
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# stem the words (e.g. convert each word to its stem, where applicable)
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()


# create a corpus (type of object expected by tm) and document term matrix
Corpus <- Corpus(VectorSource(cleaned_documents$terms)) # make a corpus object
DTM <- DocumentTermMatrix(Corpus) # get the count of words/document

# remove any empty rows in our document term matrix (if there are any 
# we'll get an error when we try to run our LDA)
unique_indexes <- unique(DTM$i) # get the index of each unique value
DTM <- DTM[unique_indexes,] # get a subset of only those indexes
# preform LDA & get the words/topic in a tidy text format
lda <- LDA(DTM, k = 40, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta")

# get the top ten terms for each topic
top_terms <- topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(10, beta) %>% # get the top 10 most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange words in descending informativeness

# Remove commas from the "term" column
top_terms <- top_terms %>%
  mutate(term = gsub(",", "", term))

write.csv(top_terms, "td_beta.csv")

# plot the top ten terms for each topic in order
top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%  # Reorder terms within each topic
  ggplot(aes(term, beta, fill = factor(topic))) +  # Plot beta by term
  geom_col(show.legend = FALSE) +  # Create bar plot without legend
  facet_wrap(~ topic, scales = "free") +  # Facet by topic
  labs(x = NULL, y = "Beta") +  # No x label, label y-axis
  coord_flip() +  # Flip bars horizontally
  scale_x_reordered() 


# Get the document-topic distribution matrix
lda_posterior <- posterior(lda)  # Extract the posterior from the LDA model

doc_topic_dist <- lda_posterior$topics  # This is the document-topic matrix

# Optional: Save the document-topic matrix to a variable for later use
document_topic_matrix <- as.data.frame(doc_topic_dist)

# Add document IDs for clarity
document_topic_matrix <- document_topic_matrix %>%
  mutate(Document = row_number()) %>%
  relocate(Document)  # Move the Document column to the front

document_topic_matrix= merge(document_topic_matrix, reviews, by="Document")


ds=document_topic_matrix

write_csv(ds, "ds.csv")
