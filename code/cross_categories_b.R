# November 23, 2021. Tue
# Ref FP, mention categories, impact strata

library(tidyverse)

frame <- read_csv("data/sample_frame.csv") %>% 
  mutate(issue_dates=str_sub(issue_dates, 3, -3)) %>% 
  mutate(issue_year=as.numeric(str_sub(issue_dates, 1, 4))) %>% 
  filter(issue_year>2015) %>% 
  mutate(mention_density_group=if_else(mention_density_group=="[0,1]",
                                       "(0,1]", mention_density_group)) %>% 
  mutate(stratum=if_else(stratum=="[1001,12895]", "[1001,12982]",
                         stratum))


frame$stratum <- factor(frame$stratum, levels=c("[1,10]", "[11,100]",
                                                "[101,1000]","[1001,12982]",
                                                "No Impact Factor")) 

all <- read_csv("data/full_coding_results.csv")

all_valid <- all %>% 
  filter(coding_id=="A1" & coding_result==1) %>% 
  distinct(sample_id) %>% 
  mutate(sample_id=str_extract(sample_id, "\\d+-\\d+-\\d+")) %>% 
  left_join(frame, by="sample_id") %>% drop_na(anno_key) %>% 
  distinct(sample_id, anno_key, doc_key, group_num, doc_num, anno_num, 
           issue_year, stratum, mention_density_group) %>% 
  filter(issue_year > 2015)

all_valid_id <- all_valid %>% distinct(sample_id) %>% pull()

all_for_join <- all %>% 
  mutate(sample_id=str_extract(sample_id, "\\d+-\\d+-\\d+"))

all_cleaned <- all_for_join %>% 
  filter(sample_id %in% all_valid_id) %>% 
  mutate(coding_result=replace_na(coding_result, 0)) %>%
  filter(!coding_id %in% c("A12", "A13", "B1", "B8", "D15", "D16", "E4"))

true_pos <- all_cleaned %>% 
  filter(coding_id %in% c("B2", "B3", "B4", "B5", "B6", "B7", 
                          "B9", "B10", "B11", "B12") ) %>% 
  select(sample_id, coding_id, coding_result) %>% 
  mutate(coding_result=as.numeric(coding_result)) %>% 
  pivot_wider(names_from=coding_id, values_from=coding_result) %>% 
  rowwise() %>% 
  mutate(sum = sum(B2,B3,B4,B5,B6,B7,B9,B10,B11,B12)) %>% 
  mutate(has_ref=if_else(sum>0, "1", "0")) %>% 
  select(sample_id, has_ref) 

true_pos %>% 
  filter(has_ref == "1") %>% View

true_pos %>% 
  left_join(frame, by="sample_id") %>% 
  filter(has_ref == "1") %>% 
  filter(is.na(tei)) %>% View
# 11 false negatives

conf_int <- prop.test(11, 210)$conf.int
round(conf_int[1], 3)
round(conf_int[2], 3)

# categorizing
categories <- all_cleaned %>%
  filter(coding_id == "A1" & coding_result == 1) %>% 
  distinct(sample_id) %>% 
  left_join(all_cleaned, by="sample_id") %>% distinct() %>% 
  select(-hint, -memo, -explanation) %>% 
  filter(!coding_id %in% c("A1", "A4", "A6", "A8", "A10", "B1", "B2")) %>% 
  mutate(category = case_when(
    coding_id == "A2" ~ "like instrument",
    coding_id == "A3" ~ "in-text name",
    coding_id == "A5" ~ "in-text version",
    coding_id == "A7" ~ "in-text publisher",
    coding_id == "A9" ~ "in-text URL",
    coding_id == "A11" ~ "configuration details", 
    coding_id == "A12" ~ "software used",
    coding_id == "A13" ~ "software not used",
    coding_id == "B3" ~ "cite to software publication",
    coding_id == "B4" ~ "cite to software",
    coding_id == "B5" ~ "cite to domain publication",
    coding_id == "B6" ~ "cite to user manual/guide", 
    coding_id == "B7" ~ "cite to a project",
    coding_id == "B9" ~ "in-reference name",
    coding_id == "B10" ~ "in-reference version",
    coding_id == "B11" ~ "in-reference URL",
    coding_id == "B12" ~ "in-reference publisher",
    coding_id == "C1" ~ "identifiable",
    coding_id == "C2" ~ "findable",
    coding_id == "C3" ~ "findable version",
    coding_id == "C4" ~ "cite to a unique, persistent identifier that points to software",
    coding_id == "C5" ~ "cite to a commit hash", 
    coding_id == "C6" ~ "no access",
    coding_id == "C7" ~ "proprietary", 
    coding_id == "C8" ~ "free access",
    coding_id == "C9" ~ "source code accessible",
    coding_id == "C10" ~ "modifiable",
    coding_id == "C11" ~ "open source licensed",
    coding_id == "D1" ~ "matched to citation request",
    coding_id == "D2" ~ "plain text citation request",
    coding_id == "D3" ~ "BibTex citation request",
    coding_id == "D4" ~ "citation request in repo README",
    coding_id == "D5" ~ "citation request on webpage",
    coding_id == "D6" ~ "CITATION file",
    coding_id == "D7" ~ "CITATION.cff",
    coding_id == "D8" ~ "CodeMeta",
    coding_id == "D9" ~ "domain-specific citation request",
    coding_id == "D10" ~ "request to cite software",
    coding_id == "D11" ~ "request to cite software publication",
    coding_id == "D12" ~ "request to cite domain publication",
    coding_id == "D13" ~ "request to cite project",
    coding_id == "D14" ~ "request to cite other research product",
    coding_id == "E1" ~ "software is archived",
    coding_id == "E2" ~ "software has unique, persistent identifier",
    coding_id == "E3" ~ "software has publicly accessible metadata",
    TRUE              ~ as.character(coding_id)
  ))  %>% 
  left_join(all_valid, by="sample_id")

mention_content <- categories %>% 
  filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
                          "B11", "B12", "C1", "C2", "C3", "D1")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(versioned=if_else(`in-text version`==1|`in-reference version`==1,
                           1, 0),
         credited=if_else(`in-text publisher`==1|`in-reference publisher`==1,
                          1, 0)) %>% 
  mutate(identifiable=as.numeric(identifiable),
         findable=as.numeric(findable),
         `findable version`=as.numeric(`findable version`),
         `configuration details`=as.numeric(`configuration details`)) %>% 
  select(sample_id, stratum, mention_density_group, identifiable, findable, versioned, 
         `findable version`, credited, `configuration details`) %>% 
  rename(c(`version findable`=`findable version`)) %>% 
  pivot_longer(cols=identifiable:`configuration details`, names_to="label", values_to="value") %>% 
  filter(value == 1) %>% 
  group_by(label) %>% 
  mutate(mention_count=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  distinct(label, mention_count) %>% 
  mutate(sum=210) %>% 
  mutate(proportion=mention_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, sum)$conf.int[1],
         conf_int_high=prop.test(mention_count, sum)$conf.int[2]) 

mention_content$label <- factor(mention_content$label,
                                levels=c("identifiable", "findable",
                                         "versioned",
                                         "version findable", "credited",
                                         "configuration details"))
mention_content %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                    position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/mention_composition.png", width=6, height=4)


mention_content_by_strata <- categories %>% 
  filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
                          "B11", "B12", "C1", "C2", "C3", "D1")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(versioned=if_else(`in-text version`==1|`in-reference version`==1,
                           1, 0),
         credited=if_else(`in-text publisher`==1|`in-reference publisher`==1,
                          1, 0)) %>% 
  mutate(identifiable=as.numeric(identifiable),
         findable=as.numeric(findable),
         `findable version`=as.numeric(`findable version`),
         `configuration details`=as.numeric(`configuration details`)) %>% 
  select(sample_id, stratum, mention_density_group, identifiable, findable, versioned, 
         `findable version`, credited, `configuration details`) %>% 
  rename(c(`version findable`=`findable version`)) %>% 
  pivot_longer(cols=identifiable:`configuration details`, names_to="label", values_to="value") %>% 
  group_by(stratum) %>% 
  mutate(mention_count_stratum=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  group_by(label, stratum, mention_count_stratum) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  mutate(proportion=mention_count/mention_count_stratum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_stratum)$conf.int[1],
         conf_int_high=prop.test(mention_count, mention_count_stratum)$conf.int[2]) 

mention_content_by_strata$label <- factor(mention_content_by_strata$label,
                                levels=c("identifiable", "findable",
                                         "versioned",
                                         "version findable", "credited",
                                         "configuration details"))
mention_content_by_strata %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(stratum)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/mention_functions_by_strata.png", width=6, height=6)


mention_content_by_density <- categories %>% 
  filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
                          "B11", "B12", "C1", "C2", "C3", "D1")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(versioned=if_else(`in-text version`==1|`in-reference version`==1,
                           1, 0),
         credited=if_else(`in-text publisher`==1|`in-reference publisher`==1,
                          1, 0)) %>% 
  mutate(identifiable=as.numeric(identifiable),
         findable=as.numeric(findable),
         `findable version`=as.numeric(`findable version`),
         `configuration details`=as.numeric(`configuration details`)) %>% 
  select(sample_id, stratum, mention_density_group, identifiable, findable, versioned, 
         `findable version`, credited, `configuration details`) %>% 
  rename(c(`version findable`=`findable version`)) %>% 
  pivot_longer(cols=identifiable:`configuration details`, names_to="label", values_to="value") %>% 
  group_by(mention_density_group) %>% 
  mutate(mention_count_density_group=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  group_by(label, mention_density_group, mention_count_density_group) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  mutate(proportion=mention_count/mention_count_density_group) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_density_group)$conf.int[1],
         conf_int_high=prop.test(mention_count, mention_count_density_group)$conf.int[2]) 

mention_content_by_density$label <- factor(mention_content_by_density$label,
                                          levels=c("identifiable", "findable",
                                                   "versioned",
                                                   "version findable", "credited",
                                                   "configuration details"))
mention_content_by_density %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(mention_density_group)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/mention_functions_by_density.png", width=6, height=4)

# citation request
cite_request <- categories %>% 
  filter(coding_id %in% c("D1", "D2", "D3", "D4", "D5", "D6", "D7",
                          "D8", "D9")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(`matched to citation request`=as.numeric(`matched to citation request`),
         `plain text citation request`=as.numeric(`plain text citation request`),
         `BibTex citation request`=as.numeric(`BibTex citation request`),
         `citation request in repo README`=as.numeric(`citation request in repo README`),
         `citation request on webpage`=as.numeric(`citation request on webpage`),
         `CITATION file`=as.numeric(`CITATION file`),
         `CITATION.cff`=as.numeric(`CITATION.cff`),
         `CodeMeta`=as.numeric(`CodeMeta`),
         `domain-specific citation request`=as.numeric(`domain-specific citation request`)) %>% 
  rowwise() %>% 
  mutate(has_citation_request=sum(`plain text citation request`,
                                  `BibTex citation request`,
                                  `citation request in repo README`,
                                  `citation request on webpage`,
                                  `CITATION file`,
                                  `CITATION.cff`,
                                  `CodeMeta`,
                                  `domain-specific citation request`)) %>% 
  mutate(`Citation request available`=if_else(has_citation_request>0, 1, 0)) %>% 
  mutate(`No citation request`=if_else(has_citation_request==0, 1, 0)) %>% 
  select(-has_citation_request, -`matched to citation request`) %>% 
  pivot_longer(cols=`plain text citation request`:`No citation request`, 
               names_to="label", values_to="value") %>% 
  group_by(stratum) %>% 
  mutate(mention_count_stratum=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_density_group) %>% 
  mutate(mention_count_density_group=n_distinct(sample_id)) %>% 
  ungroup() 

name_list <- frame %>% 
  distinct(sample_id, software_name)
tibble(all_valid_id) %>% 
  rename(sample_id="all_valid_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name) %>% View
# 155 distinct software name

a <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="citation request on webpage" ~ "Citation request on webpage",
    label=="citation request in repo README" ~ "Citation request in repo README",
    label=="CITATION file" ~ "Citation metadata",
    label=="CITATION.cff" ~ "Citation metadata",
    label=="CodeMeta" ~ "Citation metadata",
    label=="domain-specific citation request" ~ "Citation metadata",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("Citation request available", "Citation request on webpage", 
                  "Citation request in repo README", "Citation metadata")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(sum=155) %>% 
  mutate(proportion=software_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) 


a$label <- factor(a$label, levels=c("Citation request available",
                                    "Citation request on webpage",
                                    "Citation request in repo README",
                                    "Citation metadata"))
# citation request availability
a %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/cite_request.png", width=6, height=4) 


a_by_strata <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="citation request on webpage" ~ "Citation request on webpage",
    label=="citation request in repo README" ~ "Citation request in repo README",
    label=="CITATION file" ~ "Citation metadata",
    label=="CITATION.cff" ~ "Citation metadata",
    label=="CodeMeta" ~ "Citation metadata",
    label=="domain-specific citation request" ~ "Citation metadata",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("Citation request available", "Citation request on webpage", 
                      "Citation request in repo README", "Citation metadata")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label, stratum, mention_count_stratum) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count/mention_count_stratum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,mention_count_stratum)$conf.int[1],
         conf_int_high=prop.test(software_count,mention_count_stratum)$conf.int[2]) 


a_by_strata$label <- factor(a_by_strata$label, levels=c("Citation request available",
                                    "Citation request on webpage",
                                    "Citation request in repo README",
                                    "Citation metadata"))
a_by_strata %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(stratum)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=60, hjust=1))
ggsave(filename="output/new-figures-revision/cite_request_by_strata.png", width=6, height=4) 


a_by_density <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="citation request on webpage" ~ "Citation request on webpage",
    label=="citation request in repo README" ~ "Citation request in repo README",
    label=="CITATION file" ~ "Citation metadata",
    label=="CITATION.cff" ~ "Citation metadata",
    label=="CodeMeta" ~ "Citation metadata",
    label=="domain-specific citation request" ~ "Citation metadata",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("Citation request available", "Citation request on webpage", 
                      "Citation request in repo README", "Citation metadata")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label, mention_density_group, mention_count_density_group) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count/mention_count_density_group) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,mention_count_density_group)$conf.int[1],
         conf_int_high=prop.test(software_count,mention_count_density_group)$conf.int[2]) 


a_by_density$label <- factor(a_by_density$label, levels=c("Citation request available",
                                                        "Citation request on webpage",
                                                        "Citation request in repo README",
                                                        "Citation metadata"))
a_by_density %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_density_group)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=55, hjust=1))
ggsave(filename="output/new-figures-revision/cite_request_by_density.png", width=6, height=4) 


# citation request format
b <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="plain text citation request" ~ "Plain text",
    label=="BibTex citation request" ~ "BibTex",
    label=="domain-specific citation request" ~ "Domain-specific",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("No citation request", "Plain text", 
                      "BibTex", "CITATION file", "CITATION.cff",
                      "CodeMeta", "Domain-specific")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(sum=155) %>% 
  mutate(proportion=software_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) 

b$label <- factor(b$label, levels=c("No citation request",
                                    "Plain text", 
                                    "BibTex",
                                    "CITATION file",
                                    "Domain-specific"))

b %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/request_type.png", width=6, height=4) 


b_by_strata <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="plain text citation request" ~ "Plain text",
    label=="BibTex citation request" ~ "BibTex",
    label=="domain-specific citation request" ~ "Domain-specific",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("No citation request", "Plain text", 
                      "BibTex", "CITATION file", "CITATION.cff",
                      "CodeMeta", "Domain-specific")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label, stratum, mention_count_stratum) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count/mention_count_stratum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,mention_count_stratum)$conf.int[1],
         conf_int_high=prop.test(software_count,mention_count_stratum)$conf.int[2]) 

b_by_strata$label <- factor(b_by_strata$label, levels=c("No citation request",
                                    "Plain text", 
                                    "BibTex",
                                    "CITATION file",
                                    "Domain-specific"))

b_by_strata %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(stratum)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=60, hjust=1))
ggsave(filename="output/new-figures-revision/request_type_by_strata.png", width=6, height=4) 


b_by_density <- cite_request %>% 
  select(sample_id, stratum, mention_density_group, label, value, 
         mention_count_stratum, mention_count_density_group) %>% 
  mutate(label=case_when(
    label=="plain text citation request" ~ "Plain text",
    label=="BibTex citation request" ~ "BibTex",
    label=="domain-specific citation request" ~ "Domain-specific",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("No citation request", "Plain text", 
                      "BibTex", "CITATION file", "CITATION.cff",
                      "CodeMeta", "Domain-specific")) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct() %>% 
  filter(value==1) %>% 
  group_by(label, mention_density_group, mention_count_density_group) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count/mention_count_density_group) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,mention_count_density_group)$conf.int[1],
         conf_int_high=prop.test(software_count,mention_count_density_group)$conf.int[2]) 

b_by_density$label <- factor(b_by_density$label, levels=c("No citation request",
                                                        "Plain text", 
                                                        "BibTex",
                                                        "CITATION file",
                                                        "Domain-specific"))

b_by_density %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_density_group)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=50, hjust=1))
ggsave(filename="output/new-figures-revision/request_type_by_density.png", width=6, height=4)
 

# what is requested for citation
requested <- categories %>% 
  filter(coding_id %in% c("D10", "D11", "D12", "D13", "D14")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(`request to cite software`=as.numeric(`request to cite software`),
         `request to cite software publication`=as.numeric(`request to cite software publication`),
         `request to cite domain publication`=as.numeric(`request to cite domain publication`),
         `request to cite project`=as.numeric(`request to cite project`),
         `request to cite other research product`=as.numeric(`request to cite other research product`)) %>% 
  pivot_longer(cols=`request to cite software`:`request to cite other research product`, names_to="label", values_to="value") %>% 
  mutate(label=case_when(
    label=="request to cite software" ~ "Software",
    label=="request to cite software publication" ~ "Software publication",
    label=="request to cite domain publication" ~ "Domain publication",
    label=="request to cite project" ~ "Project",
    label=="request to cite other research product" ~ "Other research product",
    TRUE ~ as.character(label)
  )) %>% 
  left_join(name_list, by="sample_id") %>% 
  select(-sample_id) %>% 
  distinct(software_name, stratum, mention_density_group, label, value) %>% 
  group_by(stratum) %>% 
  mutate(n_sw_stratum=n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(mention_density_group) %>% 
  mutate(n_sw_density_group=n_distinct(software_name)) %>%
  ungroup() %>% 
  filter(value == 1) 

c <- requested %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(sum=155) %>% 
  mutate(proportion=software_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) 

c$label <- factor(c$label, levels=c("Software", 
                                    "Software publication",
                                    "Domain publication",
                                    "Project",
                                    "Other research product"))

c %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/what-requested.png", width=6, height=4)

c_by_density <- requested %>% 
  group_by(label, mention_density_group, n_sw_density_group) %>% 
  summarise(software_count_density_group=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count_density_group/n_sw_density_group) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count_density_group, n_sw_density_group)$conf.int[1],
         conf_int_high=prop.test(software_count_density_group, n_sw_density_group)$conf.int[2]) 

c_by_density$label <- factor(c_by_density$label, levels=c("Software", 
                                    "Software publication",
                                    "Domain publication",
                                    "Project",
                                    "Other research product"))

c_by_density %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1)) +
  facet_grid(cols=vars(mention_density_group)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/what_requested_by_density.png", width=6, height=4)


c_by_strata <- requested %>% 
  group_by(label, stratum, n_sw_stratum) %>% 
  summarise(software_count_stratum=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=software_count_stratum/n_sw_stratum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count_stratum,n_sw_stratum)$conf.int[1],
         conf_int_high=prop.test(software_count_stratum,n_sw_stratum)$conf.int[2]) 

c_by_strata$label <- factor(c_by_strata$label, levels=c("Software", 
                                                        "Software publication",
                                                        "Domain publication",
                                                        "Project",
                                                        "Other research product"))

c_by_strata %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1)) +
  facet_grid(cols=vars(stratum)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/what_requested_by_strata.png", width=6, height=4)


d <- categories %>% 
  filter(category %in% c("no access", "proprietary", "free access",
                         "source code accessible", "modifiable",
                         "open source licensed")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from=category, values_from=coding_result) %>% 
  rename(c(`Not accessible`="no access",
           Proprietary="proprietary",
           `Open source`="open source licensed")) %>% 
  mutate(`Non commercial`=if_else(`Not accessible`=="0" & 
                                    `Open source`=="0" & `Proprietary`=="0", 
                                  "1", "0")) %>% 
  select(-`free access`, -`source code accessible`, -modifiable) %>% 
  pivot_longer(!sample_id, names_to="category", values_to="coding_result") %>% 
  filter(coding_result==1) %>% 
  select(-coding_result) 


c %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  facet_grid(vars(category)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/request_to_cite.png", width=6, height=4)

# software metadata
metadata <- categories %>% 
  filter(coding_id %in% c("E1", "E2", "E3")) %>%
  select(-coding_scheme, -coding_id) %>% 
  mutate(category=case_when(
    category=="software is archived" ~ "Archived",
    category=="software has unique, persistent identifier" ~ "Unique and persistent identifier",
    category=="software has publicly accessible metadata" ~ "Metadata available",
    TRUE ~ as.character("NA")
  )) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(`Archived`=as.numeric(`Archived`),
         `Unique and persistent identifier`=as.numeric(`Unique and persistent identifier`),
         `Metadata available`=as.numeric(`Metadata available`)) %>% 
  pivot_longer(`Archived`:`Metadata available`, names_to="label", values_to="value") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, stratum, mention_density_group, label, value) %>% 
  group_by(stratum) %>% 
  mutate(n_sw_stratum=n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(mention_density_group) %>% 
  mutate(n_sw_density_group=n_distinct(software_name)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  group_by(label) %>% 
  mutate(software_count=n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(sum=155) %>% 
  mutate(proportion=software_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count, sum)$conf.int[1],
         conf_int_high=prop.test(software_count, sum)$conf.int[2]) 

e <- metadata %>% 
  mutate(software_count_a = sum-software_count) %>% 
  mutate(label = case_when(
    label=="Archived" ~ "Not archived",
    label=="Metadata available" ~ "No metadata",
    label=="Unique and persistent identifier" ~ "No unique and persistent identifier",
    TRUE ~ as.character("NA")
  )) %>% 
  distinct(label, software_count_a, sum) %>% 
  rename(software_count="software_count_a") %>% 
  bind_rows(metadata %>% distinct(label, software_count, sum)) %>% 
  mutate(value=case_when(
    label=="Archived" ~ "TRUE",
    label=="Not archived" ~ "FALSE",
    label=="Unique and persistent identifier" ~ "TRUE",
    label=="No unique and persistent identifier" ~ "FALSE",
    label=="Metadata available" ~ "TRUE",
    label=="No metadata" ~ "FALSE"
  )) %>% 
  mutate(label=case_when(
    label=="Not archived" ~ "Archived",
    label=="No unique and persistent identifier" ~ "Unique and persistent identifier",
    label=="No metadata" ~ "Metadata available",
    TRUE ~ as.character(label)
  )) %>% 
  mutate(proportion=software_count/sum) %>%
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) %>% 
  mutate(conf_int_low=if_else(value=="FALSE", 0, conf_int_low)) %>% 
  mutate(conf_int_high=if_else(value=="FALSE", 0, conf_int_high)) %>% 
  mutate(conf_int_low=na_if(conf_int_low, 0),
         conf_int_high=na_if(conf_int_high, 0))

# e$label <- factor(metadata$label, levels=c("archived", 
#                                     "unique and persistent identifier",
#                                     "metadata available"))

e %>% 
  ggplot(aes(x=label, y=proportion, fill=value)) +
  geom_bar(stat='identity', position='fill') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("lightgray", "darkgray"))+
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  guides(fill=guide_legend(reverse=T)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "top",
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1)) 
ggsave(filename="draft/output/standard_compliant.png", width=6, height=4)


metadata %>% 
  group_by(label, stratum, n_sw_stratum) %>% 
  mutate(sw_count_stratum=n_distinct(software_name)) %>% 
  ungroup() %>% 
  distinct(label, stratum, sw_count_stratum, n_sw_stratum) %>% 
  mutate(proportion=sw_count_stratum/n_sw_stratum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(sw_count_stratum, n_sw_stratum)$conf.int[1],
         conf_int_high=prop.test(sw_count_stratum, n_sw_stratum)$conf.int[2]) %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(stratum)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/standard_compliant_by_strata.png", width=6, height=4)


metadata %>% 
  group_by(label, mention_density_group, n_sw_density_group) %>% 
  mutate(sw_count_density_group=n_distinct(software_name)) %>% 
  ungroup() %>% 
  distinct(label, mention_density_group, sw_count_density_group, n_sw_density_group) %>% 
  mutate(proportion=sw_count_density_group/n_sw_density_group) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(sw_count_density_group, n_sw_density_group)$conf.int[1],
         conf_int_high=prop.test(sw_count_density_group, n_sw_density_group)$conf.int[2]) %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_density_group)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/standard_compliant_by_density.png", width=6, height=4)
