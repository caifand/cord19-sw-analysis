# Start date: July 2, 2021. Fri
# Parsing CORD-19 software mention extraction

library(tidyverse)
library(jsonlite)

# Prerequisite: Get the CORD-19 software extraction dataset v0.2.1 first
# https://zenodo.org/record/5140437

annotations <- read_json("/home/rstudio/cord-19-sw/data/annotations.json") 
# 318,138 records, 15 columns
annos <- annotations %>% 
  # slice_head(n=1000) %>% 
  flatten() %>% 
  unnest_wider(references, names_sep='.') %>% 
# 35 variables, still nested
  select(-ends_with("boundingBoxes")) %>% 
  unnest_wider(references.reference_id) %>%
  rename(reference_id=`$oid`) %>%
  unnest_wider(reference_id, names_sep='_')
# 31 variables

colnames(annos)[1:3] <- c("annos.key", "annos.id", "annos.rev")
colnames(annos)

# sanity check
annos %>% select(type) %>% distinct()
annos %>% 
  filter(! is.na(`software-name.confidence`)) %>% View
annos %>% 
  filter(`software-name.normalizedForm` != `software-name.rawForm`) %>% 
  View
# for human checking normalizedForm & rawForm are interchangeable
annos %>% filter(! is.na(reference_id_12)) %>% View
annos %>% 
  filter(`software-name.normalizedForm` != `software-name.rawForm`) %>% View


# look into software mentions a bit
annos %>% 
  group_by(`software-name.normalizedForm`) %>% 
  summarize(mention_count=n()) %>% 
  # 52,816 distinct extraction of software names; a bit messy
  arrange(desc(mention_count)) %>% View
# may need normalize the strings and manually sort the top mentions

# get a sample of mentions
annos_sample <- annos %>% 
  select(`software-name.normalizedForm`, 
         version.normalizedForm, 
         publisher.normalizedForm,
         url.normalizedForm,
         context) %>% 
  sample_n(size=200, replace=FALSE)
write_csv(annos_sample, "annos_sample.csv")

rm(annotations)


documents <- fromJSON("documents.json")
# part of the CORD-19 that has software mentions recognized
# 76,448 records, 13 columns
docs <- documents %>% flatten()
# 103 variables, still nested

# sanity check
docs %>% 
  filter(is.na(metadata.DOI)) %>% View
# it looks like each document has DOI/metadata.URL
# metadata.type has the following values: journal-article, book-chapter,
# posted-content, & proceedings-article
docs %>% 
  unnest(`metadata.short-title`, keep_empty = TRUE) %>% 
  select(`metadata.short-title`) %>% distinct()
# all metadata-short.title are NULL 
# There are a few metadata.subtitle available
docs %>% 
  filter(! is.na(`metadata.group-title`)) %>% 
  select(metadata.subtype) %>% distinct()
# According to CrossRef, prepublication content items may be organized
# into groupings within a given publisher
# See https://data-crossref.org/reports/help/schema_doc/4.4.2/index.html
docs %>% 
  filter(! is.na(metadata.arxiv_id)) %>%  View
docs %>% 
  filter(`metadata.container-title`=="character(0)") %>% 
  select(`metadata.short-container-title`) %>% 
  distinct() %>% View

colnames(docs)[1:3] <- c("doc.key", "doc.id", "doc.rev")
colnames(docs)

# look into publication venue
docs %>% 
  group_by(`metadata.container-title`) %>% 
  summarise(venue_freq = n()) %>% 
  arrange(desc(venue_freq)) %>% View
# roughly 7031 publication venues
docs %>% 
  group_by(metadata.type, `metadata.container-title`) %>% 
  summarise(venue_freq=n()) %>% 
  arrange(desc(metadata.type, venue_freq)) %>% View
# We might want to just analyze journal publications
# simply because other types of publication are scarce
docs %>% 
  group_by(metadata.type) %>% 
  summarise(venue_freq=n())

docs_simple <- docs %>% 
  slice_head(n=1000) %>%
  select(doc.key, doc.id, doc.rev, file_name, id, original_file_path, 
         metadata.id, metadata.pmid, metadata.pmcid, metadata.URL, 
         metadata.DOI, metadata.prefix,
         metadata.title, metadata.subtitle, metadata.page, 
         metadata.type, metadata.publisher, 
         `metadata.container-title`, `metadata.short-container-title`, 
         metadata.issue, metadata.author, metadata.subject,
         metadata.oaLink, metadata.has_valid_pdf, metadata.link, 
         metadata.assertion, `metadata.content-domain.domain`,
         `metadata.journal-issue.issue`, 
         ends_with("date-parts"),
         -starts_with("metadata.event")) %>% 
  unnest_wider(metadata.link, names_sep=".")

rm(documents)

# a small demo for checking publication time
annos_in_docs <- annos %>% 
  slice_head(n=1000) %>% 
  left_join(docs_simple, by=c("document.$oid"="doc.key")) %>% 
  select(annos.key, annos.id, doc.id,
         metadata.pmid, metadata.pmcid, 
         metadata.URL, metadata.DOI,
         `software-name.normalizedForm`, 
         version.normalizedForm, 
         publisher.normalizedForm,
         url.normalizedForm,
         context, 
         metadata.title, metadata.subtitle, metadata.type,
         `metadata.container-title`, `metadata.short-container-title`,
         metadata.issue, `metadata.journal-issue.issue`,
         metadata.page, metadata.publisher,
         metadata.subject, metadata.oaLink,
         metadata.link.URL, `metadata.content-domain.domain`,
         metadata.assertion,
         ends_with("date-parts"),
         references.normalizedForm, references.refKey,
         starts_with("reference_id"))
# now can get publication time and analyze trends of software mentions

# Some ideas:
# number of software mentions over time (and its distribution)
# avg # of mentions in publications (doc freq) over time (and distribution)
# top software mentions over time
# top journals that mention the most software over time?
# distribution of doc freq across journals over time
# emerging software (i.e., quick adoption) in CORD-19

references <- fromJSON("references.json")
# 33,720 records, 6 columns
refs <- references %>% slice_head(n=1000) %>% flatten() 
  # 6 variables

colnames(refs)[1:3] <- c("ref.key", "ref.id", "ref.rev")
colnames(refs)

rm(references)

annos_in_docs %>% 
  left_join(refs, by=c('reference_id_1'='ref.key')) %>% 
  select(-ref.rev) %>% View
# check citations from here
