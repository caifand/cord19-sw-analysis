# August 31, 2021. Tue
# Ref FP, mention categories, impact strata

library(tidyverse)
# library(GGally)

frame <- read_csv("draft/data/sample_frame.csv") %>% 
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

all <- read_csv("draft/data/full_coding_results.csv")

# james_a_assign <- read_csv("cord19-sw-analysis/docs/past_assignment/james_coding_assignment.csv") %>% 
#   select(sample_id, tei)
# james_b_assign <- read_csv("cord19-sw-analysis/docs/past_assignment/james_coding_assignment_2.csv") %>% 
#   select(sample_id, tei)
# hannah_assign <- read_csv("cord19-sw-analysis/docs/past_assignment/hannah_coding_assignment.csv") %>% 
#   select(sample_id, tei)
# fan_assign <- read_csv("cord19-sw-analysis/docs/past_assignment/fan_coding_assignment.csv") %>% 
#   select(sample_id, tei)
# agreement_assign <- read_csv("cord19-sw-analysis/docs/check_for_agreement.csv") %>% 
#   select(sample_id, tei)

# all_assign <- rbind(james_a_assign, james_b_assign, hannah_assign,
#                     fan_assign, agreement_assign) %>% 
#   mutate(tei_refs=if_else(is.na(tei), 0, 1)) %>% 
#   select(-tei) %>% 
#   group_by(sample_id) %>% 
#   summarise(tei=sum(tei_refs))

all_valid <- all %>% 
  filter(coding_id=="A1" & coding_result==1) %>% 
  distinct(sample_id) %>% 
  mutate(sample_id=str_extract(sample_id, "\\d+-\\d+-\\d+")) %>% 
  left_join(frame, by="sample_id") %>% drop_na(anno_key) %>% 
  distinct(sample_id, anno_key, doc_key, group_num, doc_num, anno_num, issue_year) %>% 
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
  ))  

mention_content <- categories %>% 
  filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
                          "B11", "B12", "C1", "C2", "C3", "D1")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  left_join(true_pos, by="sample_id") %>% 
  mutate(versioned=if_else(`in-text version`==1|`in-reference version`==1,
                           1, 0),
         credited=if_else(`in-text publisher`==1|`in-reference publisher`==1,
                          1, 0)) %>% 
  mutate(identifiable=as.numeric(identifiable),
         findable=as.numeric(findable),
         `findable version`=as.numeric(`findable version`),
         `configuration details`=as.numeric(`configuration details`)) %>% 
  select(sample_id, identifiable, findable, versioned, 
         `findable version`, credited, `configuration details`) %>% 
  rename(c(`version findable`=`findable version`)) %>% 
  pivot_longer(!sample_id, names_to="label", values_to="value") 

mention_content <- mention_content %>% 
  group_by(label) %>% 
  summarise(mention_count=sum(value)) %>% 
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
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/mention_composition.png", width=6, height=4)

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
  mutate(`citation request available`=if_else(has_citation_request>0, 1, 0)) %>% 
  select(-has_citation_request) 
  # mutate(`No citation request` = case_when(
  #   `citation request available`==1 ~ 0,
  #   `citation request available`==0 ~ 1
  # )) %>% 
  # select(-`matched to citation request`, -`citation request available`) %>% 
  # pivot_longer(!sample_id, names_to="label", values_to="value") %>% 
  # filter(value==1) 
  # group_by(label) %>% 
  # summarise(count=n_distinct(sample_id)) %>% 
  # mutate(sum=210) %>% 
  # mutate(proportion=count/sum) %>% 
  # rowwise() %>% 
  # mutate(conf_int_low=prop.test(count,sum)$conf.int[1],
  #        conf_int_high=prop.test(count,sum)$conf.int[2]) 

name_list <- frame %>% 
  distinct(sample_id, software_name)
tibble(all_valid_id) %>% 
  rename(sample_id="all_valid_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name) %>% View
# 155 distinct software name

sw_req <- cite_request %>% 
  select(sample_id, `citation request available`) %>% 
  left_join(name_list, by="sample_id") %>% 
  group_by(software_name) %>% 
  filter(`citation request available` == 1) %>% 
  distinct(software_name) %>% pull()

req_match_rights <- cite_request %>% 
  select(sample_id, `matched to citation request`) %>% 
  left_join(d, by="sample_id") %>% 
  group_by(category, `matched to citation request`) %>% 
  mutate(n_mention = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(n_mention_x_right = n_distinct(sample_id)) %>% 
  select(-sample_id) %>% distinct() %>% 
  mutate(prop=n_mention/n_mention_x_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_mention, n_mention_x_right)$conf.int[1],
         conf_int_high=prop.test(n_mention, n_mention_x_right)$conf.int[2]) %>% 
  filter(`matched to citation request` == 1 &
           category != "Not accessible") 

req_match_rights$category <- factor(req_match_rights$category, 
                                    levels=c("Proprietary", "Non commercial", 
                                             "Open source"))

req_match_rights %>% 
  ggplot(aes(x=category, y=prop)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high),width=.2,
                position=position_dodge(.9)) + 
  scale_x_discrete(name="") +
  scale_y_continuous(limits=c(0,0.9), name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/match_cite_req.png", width=6, height=4)

req_rights <- cite_request %>% 
  select(sample_id, `citation request available`) %>% 
  left_join(d, by="sample_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, category, `citation request available`) %>% 
  group_by(category, `citation request available`) %>% 
  mutate(n_sw = n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(n_sw_cat = n_distinct(software_name)) %>% 
  select(-software_name) %>% distinct() %>% 
  filter(category != "Not accessible") %>% 
  mutate(prop=n_sw/n_sw_cat) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_sw, n_sw_cat)$conf.int[1],
         conf_int_high=prop.test(n_sw, n_sw_cat)$conf.int[2]) 

req_rights$category <- factor(req_rights$category, 
                              levels=c("Proprietary", "Non commercial", 
                                             "Open source"))

req_rights %>% 
  filter(`citation request available`==1) %>% 
  ggplot(aes(x=category, y=prop)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high),width=.2,
                position=position_dodge(.9)) + 
  scale_x_discrete(name="") +
  scale_y_continuous(limits=c(0, 0.9), name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/cite_req_rights.png", width=6, height=4)


a <- cite_request %>% 
  select(sample_id, `citation request available`,
         `citation request in repo README`, `citation request on webpage`,
         `CITATION file`, `CITATION.cff`, `CodeMeta`, `domain-specific citation request`) %>% 
  pivot_longer(!sample_id, names_to='category', values_to='value') %>% 
  mutate(label=case_when(
    category=="citation request available" ~ "Citation request available",
    category=="citation request on webpage" ~ "Citation request on webpage",
    category=="citation request in repo README" ~ "Citation request in repo README",
    category=="CITATION file" ~ "Citation metadata",
    category=="CITATION.cff" ~ "Citation metadata",
    category=="CodeMeta" ~ "Citation metadata",
    category=="domain-specific citation request" ~ "Citation metadata",
    TRUE ~ as.character(category)
  )) %>% 
  left_join(name_list, by="sample_id") %>% 
  filter(value==1) %>% 
  group_by(software_name, label) %>% 
  select(-category, -sample_id) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
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
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=35, hjust=1))
ggsave(filename="draft/output/cite_request.png", width=6.5, height=4) 

# citation request format
library(waterfalls)

b <- categories %>% 
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
  mutate(`citation request available`=if_else(has_citation_request>0, 1, 0)) %>% 
  select(-has_citation_request) %>% 
  mutate(`No citation request` = case_when(
    `citation request available`==1 ~ 0,
    `citation request available`==0 ~ 1
  )) %>% 
  select(-`matched to citation request`, -`citation request available`) %>% 
  select(-`citation request in repo README`, -`citation request on webpage`) %>% 
  pivot_longer(!sample_id, names_to="label", values_to="value") %>% 
  left_join(name_list, by="sample_id") %>% 
  filter(value==1) %>% 
  distinct(software_name, label, value) %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  mutate(label=case_when(
    label=="plain text citation request" ~ "Plain text",
    label=="BibTex citation request" ~ "BibTeX",
    label=="domain-specific citation request" ~ "Domain-specific",
    TRUE ~ as.character(label)
  )) %>% 
  mutate(sum=155) %>% 
  mutate(proportion=software_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) 

b$label <- factor(b$label, levels=c("No citation request",
                                    "Plain text", 
                                    "BibTeX",
                                    "CITATION file",
                                    "Domain-specific"))

# proportion <- b$proportion 
# label <- labels=b$label
# waterfall(b) 

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
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/request_type.png", width=6, height=4) 

 # what is requested for citation
c <- categories %>% 
  filter(coding_id %in% c("D10", "D11", "D12", "D13", "D14")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(`request to cite software`=as.numeric(`request to cite software`),
         `request to cite software publication`=as.numeric(`request to cite software publication`),
         `request to cite domain publication`=as.numeric(`request to cite domain publication`),
         `request to cite project`=as.numeric(`request to cite project`),
         `request to cite other research product`=as.numeric(`request to cite other research product`)) %>% 
  pivot_longer(!sample_id, names_to="label", values_to="value") %>% 
  mutate(label=case_when(
    label=="request to cite software" ~ "Software",
    label=="request to cite software publication" ~ "Software publication",
    label=="request to cite domain publication" ~ "Domain publication",
    label=="request to cite project" ~ "Project",
    label=="request to cite other research product" ~ "Other research product",
    TRUE ~ as.character(label)
  )) %>% 
  filter(value == 1) %>% 
  left_join(d, by="sample_id") %>%
  left_join(name_list, by="sample_id") %>% 
  # distinct(software_name, label) %>% 
  # group_by(label) %>% 
  # summarise(software_n = n_distinct(software_name)) %>% 
  # ungroup() %>% 
  # mutate(sw_sum=155) %>% 
  # mutate(prop=software_n/sw_sum) %>% 
  # rowwise() %>% 
  # mutate(conf_int_low=prop.test(software_n, sw_sum)$conf.int[1],
  #        conf_int_high=prop.test(software_n, sw_sum)$conf.int[2])
  distinct(software_name, label, category) %>% 
  group_by(label, category) %>%
  summarise(software_count=n_distinct(software_name)) %>%
  ungroup() %>% 
  # group_by(category) %>%
  mutate(software_sum=case_when(
    category == "Non commercial" ~ 48,
    category == "Open source" ~ 65,
    category == "Proprietary" ~ 37,
    TRUE ~ 0)) %>% 
  mutate(proportion=software_count/software_sum) %>%
  rowwise() %>%
  mutate(conf_int_low=prop.test(software_count,software_sum)$conf.int[1],
         conf_int_high=prop.test(software_count,software_sum)$conf.int[2]) %>% 
  arrange(category)

c$label <- factor(c$label, levels=c("Software", 
                                    "Software publication",
                                    "Domain publication",
                                    "Project",
                                    "Other research product"))

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
                                  1, 0)) %>% 
  select(-`free access`, -`source code accessible`, -modifiable) %>% 
  pivot_longer(!sample_id, names_to="category", values_to="coding_result") %>% 
  filter(coding_result==1) %>% 
  select(-coding_result) 

c$category <- factor(c$category, levels=c("Proprietary", "Non commercial",
                                          "Open source"))

c %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  facet_grid(vars(category)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1), breaks=c(0,0.5,1)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/request_to_cite.png", width=4, height=5)

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
  pivot_longer(!sample_id, names_to="label", values_to="value") %>% 
  filter(value == 1) %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, label, value) %>% 
  group_by(label) %>% 
  summarise(software_count=n_distinct(software_name)) %>% 
  mutate(sum=155) 

e <- metadata %>% 
  mutate(software_count_a = sum-software_count) %>% 
  mutate(label = case_when(
    label=="Archived" ~ "Not archived",
    label=="Metadata available" ~ "No metadata",
    label=="Unique and persistent identifier" ~ "No unique and persistent identifier",
    TRUE ~ as.character("NA")
  )) %>% 
  select(label, software_count_a, sum) %>% 
  rename(software_count="software_count_a") %>% 
  bind_rows(metadata) %>% 
  mutate(category=case_when(
    label=="Not archived" ~ "Archived",
    label=="No unique and persistent identifier" ~ "Unique and persistent identifier",
    label=="No metadata" ~ "Metadata available",
    TRUE ~ as.character(label)
  )) %>% 
  mutate(value=case_when(
    label=="Archived" ~ "TRUE",
    label=="Not archived" ~ "FALSE",
    label=="Unique and persistent identifier" ~ "TRUE",
    label=="No unique and persistent identifier" ~ "FALSE",
    label=="Metadata available" ~ "TRUE",
    label=="No metadata" ~ "FALSE"
  )) %>% 
  mutate(proportion=software_count/sum) %>%
  rowwise() %>% 
  mutate(conf_int_low=prop.test(software_count,sum)$conf.int[1],
         conf_int_high=prop.test(software_count,sum)$conf.int[2]) %>% 
  mutate(conf_int_low=if_else(value=="FALSE", 0, conf_int_low)) %>% 
  mutate(conf_int_high=if_else(value=="FALSE",0, conf_int_high)) 

e$value <- factor(e$value, levels=c("FALSE", "TRUE"))

e$category <- factor(e$category, levels=c("Unique and persistent identifier", 
                                    "Metadata available", 
                                    "Archived"))

e %>% 
  ggplot(aes(y=category, x=proportion, fill=value)) +
  geom_bar(stat='identity', position='fill') +
  geom_errorbar(aes(xmin=conf_int_low, xmax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("lightgray", "darkgray"), labels=c("FALSE", "TRUE"))+
  scale_y_discrete(name="") +
  scale_x_continuous(name="Proportion") +
  guides(fill=guide_legend(reverse=T)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=14),
        legend.position = "bottom",
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1)) 
ggsave(filename="draft/output/standard_compliant.png", width=6, height=4)

# now upset chart
# library(UpSetR)
# 
# mention_elements <- categories %>% 
#   filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
#                           "B11", "B12")) %>%
#   select(-coding_scheme, -coding_id) %>% 
#   pivot_wider(names_from="category", values_from="coding_result") %>% 
#   mutate(`software name`=if_else(`in-text name`==1|`in-reference name`==1, 1, 0),
#          version=if_else(`in-text version`==1|`in-reference version`==1, 1, 0),
#          publisher=if_else(`in-text publisher`==1|`in-reference publisher`==1, 1, 0),
#          url=if_else(`in-text URL`==1|`in-reference URL`==1, 1, 0)) %>% 
#   select(sample_id, `software name`, version, publisher, url, `configuration details`) 
# 
# software_name <- sum(mention_elements$`software name`)
# mention_elements %>% 
#   filter(`software name`==1&version==1&publisher==1&url==1)
# 
# input <- c(
#   software = 56,
#   "software&version" = 33,
#   "software&publisher" = 27,
#   "software&url" = 4,
#   "software&version&publisher" = 29,
#   "software&version&url" = 3,
#   "software&publisher&url" = 4,
#   "software&version&publisher&url" = 1)
# 
# upset <- upset(fromExpression(input), 
#                    nintersects = 8, 
#                    nsets = 4, 
#                    order.by = "freq", 
#                    decreasing = T, 
#                    mb.ratio = c(0.68, 0.32),
#                    text.scale = 2.5,
#                    point.size = 5,
#                    line.size = 1.2,
#                    mainbar.y.max = 80,
#                    mainbar.y.label = "Number of Mentions",
#                    sets.x.label = "Total of times\nbeing Mentioned",
#                    set_size.numbers_size = 6.5,
#                    set_size.show = T,
#                    set_size.scale_max = 200)
# upset

# now introduce strata
sample <- read_csv("data/sample.csv") %>% 
  distinct(sample_id, stratum, mention_density_group)

by_strata <- categories %>% 
  filter(coding_id %in% c("A3", "A5", "A7", "A9", "A11", "B9", "B10",
                          "B11", "B12", "C1", "C2", "C3", "D1")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  left_join(true_pos, by="sample_id") %>% 
  mutate(versioned=if_else(`in-text version`==1|`in-reference version`==1,
                           1, 0),
         credited=if_else(`in-text publisher`==1|`in-reference publisher`==1,
                          1, 0),
         `access given`=if_else(`in-text URL`==1|`in-reference URL`==1,
                                1, 0)) %>% 
  mutate(identifiable=as.numeric(identifiable),
         findable=as.numeric(findable),
         `findable version`=as.numeric(`findable version`),
         `configuration details`=as.numeric(`configuration details`),
         has_ref=as.numeric(has_ref),
         `matched to citation request`=as.numeric(`matched to citation request`)) %>% 
  select(sample_id, identifiable, findable, `access given`, versioned, 
         `findable version`, credited, `configuration details`, has_ref,
         `matched to citation request`) %>% 
  rename(c(`version findable`=`findable version`, 
           `formal citation`=has_ref)) %>% 
  pivot_longer(!sample_id, names_to="label", values_to="value") %>% 
  mutate(sample_id=str_sub(sample_id, 3 ,-3)) %>%
  left_join(sample, by="sample_id") %>% 
  mutate(value = case_when(
    value == 1 ~ TRUE,
    value == 0 ~ FALSE
  ))
  # filter(value == 1) 
by_strata %>% glimpse()
by_strata$mention_density_group <- factor(by_strata$mention_density_group,
                                          levels=c("[0,1]", "[2,8]",
                                                   "[9,350]"))
  
  # group_by(stratum, mention_density_group, label) %>% 
  # summarise(mention_count=sum(value)) %>% 
  # mutate(sum=15) %>% 
  # mutate(proportion=mention_count/sum) %>% 
  # rowwise() %>% 
  # mutate(conf_int_low=prop.test(mention_count, sum)$conf.int[1],
         # conf_int_high=prop.test(mention_count, sum)$conf.int[2]) %>%
  # compute conf int within collapsed groups separately

by_strata %>% 
  ggplot(aes(x=label, fill=value, by=vars(mention_density_group, stratum))) +
  geom_bar(position="fill") +
  geom_text(stat='prop', position=position_fill(.5)) +
  # geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1)) +
  facet_grid(cols=vars(mention_density_group), rows=vars(stratum), margins=TRUE)
  
  
ggsave(filename="output/software_x_mention_type_by_strata.png", width=6, height=4)

