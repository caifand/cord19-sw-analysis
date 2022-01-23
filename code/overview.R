# August 31, 2021. Tue
# Overview

library(tidyverse)
library(janitor)
library(kableExtra)

sample <- read_csv("data/sample.csv")

jif <- read_csv("data/jif_cleaned.csv")

# CORD-19 overview
cord19_metadata <- read_csv("data/cord19-2020-03-22-metadata.csv")
cord19_metadata_a <- read_csv("data/cord-19-2020-03-16-metadata.csv")

cord19_metadata %>% group_by(sha) %>% 
  summarise(paper_n = n_distinct(cord_uid)) %>% 
  arrange(desc(paper_n))

cord19_metadata %>% distinct(cord_uid)

cord19_metadata %>% distinct() %>% 
  mutate(row_num = row_number()) %>% 
  mutate(publish_year=str_sub(publish_time, 1, 4) %>% as.numeric()) %>% 
  drop_na(publish_year) %>% 
  group_by(publish_year) %>% 
  summarise(paper_count=n_distinct(row_num)) %>% 
  mutate(proportion=round(paper_count/sum(paper_count),2)) %>% View

cord19_metadata %>% distinct() %>% 
  mutate(row_num = row_number()) %>% 
  mutate(publish_year=str_sub(publish_time, 1, 4) %>% as.numeric()) %>% 
  drop_na(publish_year) %>% 
  group_by(publish_year) %>% 
  summarise(paper_count=n_distinct(row_num)) %>% View
  ggplot(aes(x=publish_year, y=paper_count)) +
  geom_line() +
  scale_y_continuous(name="Number of papers", limits=c(0, 320000),
                     labels=c("0", "100000", "200000", "300000")) +
  scale_x_continuous(name="Publication year")
ggsave(filename="output/cord19_paper_by_year.png", width=6, height=4)

cord19_metadata %>% distinct() %>%
  mutate(row_num = row_number()) %>%
  mutate(publish_year=str_sub(publish_time, 1, 4) %>% as.numeric()) %>%
  drop_na(publish_year) %>%
  mutate(year_range=cut(publish_year, breaks=c(1, 2000, 2010, 2015, 2022),
                                                labels=c("[,2000]","[2001,2010]",
                                                         "[2011,2015]","[2016,2021]"))) %>%
  group_by(year_range) %>%
  summarise(paper_count=n_distinct(row_num))
# 
# paper_by_year %>% 
#   ggplot(aes(x=year_range, y=paper_count)) +
#   geom_bar(stat='identity', fill='darkgray') +
#   scale_y_continuous(name="Number of papers", limits=c(0, 430000),
#                      labels=c("0", "100000", "200000", "300000", "400000")) +
#   scale_x_discrete(name="Year range") +
#   geom_text(aes(label=paper_count, x=year_range, y=paper_count), 
#             position=position_dodge(width=0.8), vjust=-0.6, size=3.4)
# ggsave(filename="output/cord19_paper_by_year.png", width=6, height=4)

# mention density, time, strata
sample %>% 
  mutate(issue_dates=str_sub(issue_dates, 3, -3)) %>% 
  mutate(issue_year = str_sub(issue_dates, 1, 4)) %>% 
  distinct(doc_key, mention_density) %>% 
  group_by(mention_density) %>% 
  summarise(doc_count=n_distinct(doc_key)) %>% 
  ggplot(aes(x=mention_density, y=doc_count)) + 
  geom_bar(stat='identity')

sample %>% 
  distinct(doc_key, stratum, mention_density) %>% 
  group_by(stratum) %>% 
  summarise(mean_mention_density=mean(mention_density)) %>% View

sample %>% 
  distinct(doc_key, mention_density, stratum, mention_density_group, group_num) %>% 
  group_by(group_num, mention_density_group, stratum) %>% 
  mutate(avg_mention_density = mean(mention_density)) %>% 
  distinct(group_num, mention_density_group, stratum, avg_mention_density) %>% View

# now look at the whole set

# full <- read_csv("data/softcite-kb-cord19-v0.2.1.csv")
sf <- read_csv("draft/data/sample_frame.csv")
full <- sf %>% 
  select(-references, -ref_id_list, -refs, -ref_id, -ref_key, -tei, -refKey, 
         -ref_doc_key, -context) %>% 
  distinct() %>% 
  mutate(issue_dates = str_sub(issue_dates, 3, -3)) %>% 
  mutate(issue_year = str_sub(issue_dates, 1, 4)) %>% 
  mutate(stratum=if_else(stratum=="[1001,12895]", "[1001,12982]", stratum))

full %>% distinct(stratum)

full$stratum <- factor(full$stratum, levels=c("[1,10]", "[11,100]",
                                              "[101,1000]", "[1001,12982]",
                                              "No Impact Factor"))

full %>% 
  mutate(issue_year=as.integer(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year>2015) %>%
  distinct(doc_key, mention_density, issue_year) %>% 
  mutate(avg_mention_density=mean(mention_density),
         sd_mention_density=sd(mention_density)) %>% 
  View

full %>% 
  mutate(issue_year=as.integer(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year>2015) %>%
  distinct(doc_key, mention_density, issue_year) %>% 
  group_by(mention_density) %>%
  mutate(n_doc=n_distinct(doc_key)) %>% 
  distinct(mention_density, n_doc) %>% 
  ungroup() %>% 
  mutate(all_doc=sum(n_doc)) %>% 
  filter(mention_density > 8) %>% 
  mutate(num_doc=sum(n_doc)) %>% View
  
full %>% 
  mutate(issue_year=as.integer(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year>2015) %>%
  distinct(doc_key, mention_density, issue_year) %>% 
  group_by(mention_density) %>%
  mutate(n_doc=n_distinct(doc_key)) %>% 
  distinct(mention_density, n_doc) %>% 
  ungroup() %>% 
  arrange(desc(mention_density)) %>%
  ggplot(aes(x=mention_density, y=n_doc)) +
  geom_line() +
  # geom_line(aes(x=issue_year, y=avg_mention_density)) +
  scale_x_log10(limits=c(0.9,350), breaks=c(1, 10, 100, 350),
                name="Mention density per article") +
  scale_y_log10(limits=c(0.4,50000), breaks=c(1, 10, 100, 1000, 10000, 50000),
                name="Number of articles mentioning software") 
ggsave(filename="paper_by_density.png", width=6, height=4)

full %>% 
  mutate(issue_year=as.integer(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year>2015) %>%
  distinct(doc_key, mention_density) %>% 
  arrange(mention_density) %>% View
  ggplot(aes(x=mention_density)) +
  geom_histogram() +
  # geom_line(aes(x=issue_year, y=avg_mention_density)) +
  scale_x_log10(limits=c(0.9,350), breaks=c(1, 10, 100, 350),
                name="Mention density per article") +
  scale_y_log10(limits=c(0.4,50000), breaks=c(1, 10, 100, 1000, 10000, 50000),
                name="Number of articles mentioning software") 
  
full %>%
  mutate(issue_year=as.integer(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year>2015) %>%
  distinct(doc_key, stratum, mention_density_group) %>% 
  rename("Impact strata"=stratum, "Mention density"=mention_density_group) %>%
  tabyl(`Impact strata`, `Mention density`) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting(rounding="half up", digits=2) %>% 
  adorn_ns() %>% 
  adorn_title("combined") %>% 
  knitr::kable(booktabs=T, format="latex", align="c") 
  
papers_by_year <- full %>% 
  distinct(doc_key, issue_year) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  # filter(is.na(issue_year))
# 7 NA records
  drop_na() %>% 
  mutate(year_range = cut(issue_year, breaks=c(1969, 2000, 2010, 2015, 2022),
                          labels=c("[1970,2000]","[2001,2010]",
                                   "[2011,2015]","[2016,2021]"))) %>% 
  group_by(year_range) %>% 
  summarise(doc_count=n_distinct(doc_key)) 
  
papers_by_year %>% 
  ggplot(aes(x=year_range,y=doc_count)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_text(aes(label=doc_count, x=year_range, y=doc_count),
            position=position_dodge(width=0.4), vjust=-0.4, size=3) +
  scale_x_discrete(name="Year range") +
  scale_y_continuous(name="Number of articles mentioning software")
ggsave(filename="output/softcite_kb_cord19_paper_by_year.png", width=6, height=4)


# by year frequency bar chart for the last bin
full %>% 
  distinct(doc_key, issue_year) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  drop_na() %>% 
  filter(issue_year > 2015 & issue_year < 2022) %>% 
  group_by(issue_year) %>% 
  summarise(doc_count=n_distinct(doc_key)) %>% 
  ggplot(aes(x=issue_year, y=doc_count)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_text(aes(label=doc_count, x=issue_year, y=doc_count),
            position=position_dodge(width=0.4), vjust=-1, size=3) +
  scale_x_continuous(name="Publication year", breaks=c(2016, 2017, 2018,
                                                     2019, 2020, 2021)) +
  scale_y_continuous(name="Number of articles mentioning software", 
                     limits=c(0,40000))
ggsave(filename="output/recent_years.png", width=6, height=4)

doc_by_strata <- full %>% 
  distinct(doc_key, issue_year, stratum) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year < 2022) %>%
  mutate(year_range = cut(issue_year, breaks=c(1969, 2000, 2010, 2015, 2022),
                          labels=c("[1970,2000]","[2001,2010]",
                                   "[2011,2015]","[2016,2021]"))) %>%
  group_by(stratum, year_range) %>% 
  mutate(doc_count=n_distinct(doc_key)) 

doc_by_strata %>% 
  ggplot(aes(x=year_range, y=doc_count)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_text(aes(label=doc_count, x=year_range, y=doc_count),
            position=position_dodge(width=0.4), vjust=-1, size=3) +
  facet_grid(vars(stratum)) +
  scale_x_discrete(name="Year range") +
  scale_y_continuous(name="Number of articles mentioning software", 
                     labels=c("0", "10000", "20000", "30000", "40000")) 
ggsave(filename="output/papers_by_year_x_strata.png", width=6, height=6)


full %>% 
  distinct(doc_key, mention_density, issue_year) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  drop_na(issue_year) %>% 
  filter(issue_year<2022) %>% 
  group_by(issue_year) %>% 
  summarise(avg_mention_density=mean(mention_density),
            doc_count=n_distinct(doc_key)) %>% 
  # mutate(year_range = cut(issue_year, breaks=c(1969, 2000, 2010, 2015, 2022),
  #                         labels=c("[1970,2000]","[2001,2010]",
  #                                  "[2011,2015]","[2016,2021]"))) %>%
  ggplot(aes(x=issue_year, y=avg_mention_density)) +
  geom_line() +
  scale_x_continuous(name="Publication year") +
  scale_y_continuous(name=" Average mention density per article", limits=c(0,5)) 
ggsave(filename="output/softcite_kb_cord19_mention_density_by_year.png", width=6, height=4)
  
                      
mention_density_by_year <- full %>% 
  distinct(doc_key, mention_density, issue_year) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  filter(issue_year < 2022) %>% 
  group_by(issue_year) %>% 
  summarise(avg_mention_density=round(mean(mention_density),3)) %>% 
  ungroup() 

mention_density_by_year %>% 
  ggplot(aes(x=issue_year, y=avg_mention_density)) +
  geom_line() +
  scale_x_continuous(name="Publication year") +
  scale_y_continuous(name="Average mention density per article", limits=c(0,4))
# show overall

full %>% 
  distinct(doc_key, mention_density, issue_year, stratum) %>% 
  mutate(issue_year = as.numeric(issue_year)) %>% 
  drop_na() %>% 
  # filter(issue_year < 2022) %>% 
  group_by(stratum, issue_year) %>% 
  summarise(avg_mention_density=mean(mention_density)) %>% 
  ggplot(aes(x=issue_year, y=avg_mention_density)) +
  geom_line() +
  facet_grid(vars(stratum)) +
  scale_x_continuous(name="Publication year", breaks=c(1970, 1980, 1990, 
                                                       2000, 2010, 2021)) +
  scale_y_continuous(name="Average mention density per article")
ggsave(filename="output/mention_density_by_year_x_strata.png", width=6, height=6)

# box plot by year range
# geom_point() with jitter and opacity

# look into full dataset
sf %>% 
  filter(!is.na(jif)) %>% 
  distinct(doc_key)

sf %>% distinct(doc_key)


library(UpSetR)

sf %>% 
  distinct(anno_key, software_name, version, publisher, url) %>% 
  filter(!is.na(software_name)) %>% 
  filter(is.na(version)) %>% 
  filter(is.na(publisher)) %>% 
  filter(is.na(url))

input <- c(
  software = 148829,
  "software&version" = 59914,
  "software&publisher" = 21992,
  "software&url" = 18959,
  "software&version&publisher" = 36958,
  "software&version&url" = 6103,
  "software&publisher&url" = 1630,
  "software&version&publisher&url" = 1224)

upset <- upset(fromExpression(input), 
               nintersects = 8, 
               nsets = 4, 
               order.by = "freq", 
               decreasing = T, 
               mb.ratio = c(0.64, 0.36),
               text.scale = 2.1,
               point.size = 4.8,
               line.size = 1.2,
               mainbar.y.max = 180000,
               mainbar.y.label = "Number of Mentions",
               sets.x.label = "Total of times\nbeing Mentioned",
               set_size.numbers_size = 6.1,
               set_size.show = T,
               set_size.scale_max = 2000000)
upset
ggsave(filename="output/upset.png", width=18, height=12)
         