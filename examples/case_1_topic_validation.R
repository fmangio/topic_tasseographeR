
# =============================================================================
# Topic tasseographeR — Example 1 (Topic interpretation & validation)
# Replication script: STM (K = 20) + export inputs for Topic tasseographeR
# Produces:
#   - td_beta.csv (topic–word distribution) for Topic Content app
#   - ds.csv      (document–topic distribution + Text) for Topic Function app
#   - top words plots, top documents tables, and session info
#
# How to run (from repo root in RStudio):
#   install.packages("renv"); renv::restore()
#   source("examples/topic_validation.R")
# =============================================================================

set.seed(12345)
#--------------------------Install required libraries 

#install.packages("quanteda")   #(if not installed.Run only the first time)
#install.packages("tidyverse") #(if not installed.Run only the first time)
#install.packages("stm")       #(if not installed.Run only the first time)
#install.packages("tidytext")  #(if not installed.Run only the first time)
#install.packages("dplyr")    #(if not installed.Run only the first time)
#install.packages("renv")    #(if not installed.Run only the first time)

#--------------------------Load required libraries
library(quanteda)
library(tidyverse)
library(stm)
library(tidytext)
library(dplyr)
library(renv)

# ---------------------------- Paths ------------------------------------------
# Script assumes you run from the repo root (recommended: open tasseographeR.Rproj)
root <- getwd()

out_dir <- file.path(root, "examples", "case1", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_dir <- file.path(root, "examples", "case1", "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

#--------------------------load data, available at: https://github.com/dondealban/learning-stm/blob/master/data/poliblogs2008.csv

data <- read.csv("poliblogs2008.csv")  #specify your working directory

#--------------------------Topic Modelling: Replicate same modeling choices as in https://warin.ca/shiny/stm/

#pre-process
processed <- textProcessor(data$documents, metadata = data) 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#prepare

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

#estimate a 20-topic solution (k= 20), where topic prevalence is a function of review rating and spling of review posting day

poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence = ~rating + s(day), max.em.its = 75, data = out$meta, init.type = "Spectral")

#---------------------------Topic Labelling & Validation


#-------extract the word-topic probability distribution, "td_beta" (in csv)

td_beta <- tidy(poliblogPrevFit)

write_csv(td_beta, "td_beta.csv")


##-------extract the document-topic probability distribution, "ds" (in csv)


ds <-make.dt(poliblogPrevFit, meta)  

ds$Text<-ds$documents # create a "Text" column. This can also be done directly in the csv file

ds= ds %>% dplyr::select(!documents) %>% #remove useless columns. This can also be done directly in the csv file
  select(!docname) %>% 
  select(!rating) %>% 
  select(!blog) %>% 
  select(!X) %>% 
  select(!day)
  
write_csv(ds, "ds.csv")



####------------visualize 10 highest probability words for topic 5, 10, 20

td_beta %>%    
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)


## topic 5's top words:

labelTopics(poliblogPrevFit, c(5)) # they mostly deal with elections


## topic 10's top words:

labelTopics(poliblogPrevFit, c(10)) # they mostly deal with legal issues

## topic 20's top words:

labelTopics(poliblogPrevFit, c(20)) # they mostly deal with religious issues

####------------investigate exemplary documents for  topic 5, 10, 20


top5_docs <- ds %>%
  arrange(desc(Topic5)) %>%
  slice_head(n = 10) %>%
  select(Topic5, Text)

top5_docs    #they discuss election-related issues 


top10_docs <- ds %>%
  arrange(desc(Topic10)) %>%
  slice_head(n = 10) %>%
  select(Topic20, Text)

top10_docs    #they discuss legal issues. 


top20_docs <- ds %>%
  arrange(desc(Topic20)) %>%
  slice_head(n = 10) %>%
  select(Topic20, Text)

top20_docs    #they discuss religious issues. 