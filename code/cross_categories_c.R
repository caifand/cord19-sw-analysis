# November 23, 2021. Tue
# Cross categories analysis

library(tidyverse)

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
all %>% group_by(sample_id) %>% 
  summarise(row_n = n()) %>% 
  arrange(desc(row_n)) %>% View

all_cleaned <- all_for_join %>% 
  filter(sample_id %in% all_valid_id) %>% 
  mutate(coding_result=replace_na(coding_result, 0)) %>%
  filter(!coding_id %in% c("A12", "A13", "B1", "B8", "D15", "D16", "E4")) 

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
    coding_id == "D12" ~ "request to cite domain science publication",
    coding_id == "D13" ~ "request to cite project",
    coding_id == "D14" ~ "request to cite other research product",
    coding_id == "E1" ~ "software is archived",
    coding_id == "E2" ~ "software has unique, persistent identifier",
    coding_id == "E3" ~ "software has publicly accessible metadata",
    TRUE              ~ as.character(coding_id)
  )) %>% 
  left_join(all_valid, by="sample_id")


# mention types
condensed_categories <- categories %>% 
  filter(category == "like instrument") %>% 
  bind_rows(categories %>% filter(category == "cite to domain publication")) %>% 
  bind_rows(categories %>% filter(category == "cite to software publication")) %>% 
  bind_rows(categories %>% filter(category == "cite to software")) %>% 
  bind_rows(categories %>% filter(category == "in-text name")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(label = case_when(
    `cite to software` == 1 ~ "Cite to software",
    `like instrument` == 1 ~ "Like instrument",
    `cite to domain publication` == 1 ~ "Cite to publication",
    `cite to software publication` == 1 ~ "Cite to publication",
    `in-text name` == 1 ~ "Informal",
    TRUE ~ as.character(sample_id),
  )) %>% 
  distinct(sample_id, label) 

mention_functions <- categories %>% 
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
  select(sample_id, identifiable, findable, versioned, 
         `findable version`, credited, `configuration details`) %>% 
  rename(c(`version findable`=`findable version`)) %>% 
  pivot_longer(cols=identifiable:`configuration details`, names_to="label", values_to="value") %>% 
  filter(value == 1) %>% 
  distinct(sample_id, label)

mention_function_by_types <- condensed_categories %>% 
  left_join(mention_functions, by="sample_id") %>% 
  rename(mention_type=label.x, mention_function=label.y) %>% 
  mutate(mention_function=replace_na(mention_function, "not identifiable")) %>% 
  mutate(mention_category=if_else(mention_type %in% c("Informal", "Like instrument"),
                                  "Informal mentions", "Formal citations")) %>% 
  group_by(mention_type) %>% 
  mutate(mention_count_by_type = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_function, mention_type, mention_count_by_type) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  arrange(mention_function) %>% 
  filter(mention_function != "not identifiable") %>% 
  mutate(proportion=mention_count/mention_count_by_type) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_by_type)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(mention_count, mention_count_by_type)$conf.int[2])

mention_function_by_categories <- condensed_categories %>% 
  left_join(mention_functions, by="sample_id") %>% 
  rename(mention_type=label.x, mention_function=label.y) %>% 
  mutate(mention_function=replace_na(mention_function, "not identifiable")) %>% 
  mutate(mention_category=if_else(mention_type %in% c("Informal", "Like instrument"),
                                  "Informal mentions", "Formal citations")) %>% 
  group_by(mention_category) %>% 
  mutate(mention_count_by_category = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_function, mention_category, mention_count_by_category) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  arrange(mention_function) %>% 
  filter(mention_function != "not identifiable") %>% 
  mutate(proportion=mention_count/mention_count_by_category) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_by_category)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(mention_count, mention_count_by_category)$conf.int[2])

# mention_function_by_types$mention_type <- factor(mention_function_by_types$mention_type,
                                               levels=c("Cite to software",
                                                        "Cite to publication",
                                                        "Like instrument",
                                                        "Informal"))

mention_function_by_categories$mention_function <- factor(mention_function_by_categories$mention_function,
                                                  levels=c("identifiable",
                                                           "findable",
                                                           "versioned",
                                                           "version findable",
                                                           "credited",
                                                           "configuration details"))

mention_function_by_categories %>% 
  ggplot(aes(x=mention_function, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2, 
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_category)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=40, hjust=1))
ggsave(filename="draft/output/new-figures-revision/mention_function_by_categories.png", width=7, height=3.4)


mention_type_by_functions <- mention_functions %>% 
  full_join(condensed_categories, by="sample_id") %>% 
  rename(mention_function=label.x, mention_type=label.y) %>% 
  mutate(mention_function=replace_na(mention_function, "not identifiable")) %>% 
  group_by(mention_function) %>% 
  mutate(mention_count_by_function = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_type, mention_function, mention_count_by_function) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  filter(mention_function != "not identifiable") %>% 
  mutate(proportion=mention_count/mention_count_by_function) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_by_function)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(mention_count, mention_count_by_function)$conf.int[2]) %>% 
  arrange(mention_function)

mention_function_by_types$mention_type <- factor(mention_function_by_types$mention_type,
                                                  levels=c("Cite to software",
                                                           "Cite to publication",
                                                           "Like instrument",
                                                           "Informal"))

mention_function_by_types$mention_function <- factor(mention_function_by_types$mention_function,
                                                      levels=c("identifiable",
                                                               "findable",
                                                               "versioned",
                                                               "version findable",
                                                               "credited",
                                                               "configuration details"))

mention_function_by_types %>% 
  ggplot(aes(x=mention_function, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2, 
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_type)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=40, hjust=1))
ggsave(filename="output/new-figures-revision/mention_functions_by_types.png", width=7, height=3.4)

mention_type_by_functions %>% 
  ggplot(aes(x=mention_type, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2, 
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(cols=vars(mention_function)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=40, hjust=1))

# Property rights 
mentioned_rights <- categories %>% 
  filter(category %in% c("no access", "proprietary", "free access",
                         "source code accessible", "modifiable",
                         "open source licensed")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from=category, values_from=coding_result) %>% 
  rename(`Not accessible`=`no access`, `Open source`=`open source licensed`,
         Proprietary=proprietary) %>% 
  mutate(`Non commercial`=if_else(`Not accessible`=="0" & 
                                    `Open source`=="0" & `Proprietary`=="0", 
                                  1, 0)) %>% 
  select(-`free access`, -`source code accessible`, -modifiable) %>% 
  pivot_longer(cols=`Not accessible`:`Non commercial`, names_to="category", values_to="coding_result") %>% 
  filter(coding_result==1) %>% 
  select(sample_id, category) %>% 
  rename(software_right = category)

mention_type_by_right <- mentioned_rights %>% 
  left_join(condensed_categories, by="sample_id") %>% 
  rename(mention_type=label) %>% 
  group_by(software_right) %>% 
  mutate(mention_count_by_right = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_type, software_right, mention_count_by_right) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% 
  ungroup() %>% 
  arrange(software_right) %>% 
  filter(software_right != "Not accessible") %>% 
  mutate(proportion=mention_count/mention_count_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_by_right)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(mention_count, mention_count_by_right)$conf.int[2]) 


mention_type_by_right$software_right <- factor(mention_type_by_right$software_right,
                                    levels=c("Proprietary",
                                             "Non commercial", "Open source"))

mention_type_by_right$mention_type <- factor(mention_type_by_right$mention_type,
                                               levels=c("Cite to software",
                                                        "Cite to publication",
                                                        "Like instrument", "Informal"))

mention_type_by_right %>% 
  ggplot(aes(x=mention_type, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion", limits=c(0,1), breaks=c(0,0.5,1)) +
  facet_grid(rows=vars(software_right)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/mention_types_by_rights.png", width=4, height=5)


# software accessibility
accessibility <- categories %>% 
  filter(category %in% c("no access", "proprietary",
                         "free access", "source code accessible", "modifiable")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(accessible=if_else(`no access`==0, 1, 0)) %>% 
  select(-`no access`, -proprietary) %>% 
  mutate(`free access`=as.numeric(`free access`),
         `source code accessible`=as.numeric(`source code accessible`),
         modifiable=as.numeric(modifiable)) %>% 
  rename(`source code modifiable`=modifiable) %>% 
  pivot_longer(cols=`free access`:`accessible`, names_to='label', values_to='value') %>% 
  filter(value == 1) %>% 
  distinct(sample_id, label) 

mention_functions_by_rights <- mention_functions %>% 
  full_join(mentioned_rights, by="sample_id") %>% 
  rename(mention_function=label) %>% 
  group_by(software_right) %>% 
  mutate(mention_count_by_right=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(mention_function, software_right, mention_count_by_right) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  filter(software_right != "Not accessible") %>% 
  filter(!is.na(mention_function)) %>% 
  mutate(proportion=mention_count/mention_count_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_count_by_right)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(mention_count, mention_count_by_right)$conf.int[2]) 


mention_functions_by_rights$software_right <- factor(mention_functions_by_rights$software_right, 
                                     levels=c("Proprietary", "Non commercial", 
                                              "Open source"))

mention_functions_by_rights$mention_function <- factor(mention_functions_by_rights$mention_function, 
                                                     levels=c("identifiable", "findable", 
                                                              "versioned", "version findable",
                                                              "credited", "configuration details"))

mention_functions_by_rights %>% 
  ggplot(aes(x=mention_function, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(software_right)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/mention_functions_by_rights.png", width=6, height=4)

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
  distinct(sample_id, label, value)

name_list <- frame %>% 
  distinct(sample_id, software_name) 

cite_request_location <- cite_request %>% 
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
  filter(value==1) %>% 
  distinct(sample_id, label) 
  
cite_request_loc_by_rights <- mentioned_rights %>% 
  left_join(cite_request_location, by="sample_id") %>% 
  mutate(label=replace_na(label, "No citation request")) %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, software_right, label) %>% 
  group_by(software_right) %>% 
  mutate(n_sw_by_right=n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(label, software_right, n_sw_by_right) %>% 
  mutate(n_sw=n_distinct(software_name)) %>% 
  ungroup() %>% 
  distinct(software_right, label, n_sw, n_sw_by_right) %>% 
  filter(software_right != "Not accessible") %>% 
  filter(label != "No citation request") %>% 
  mutate(proportion=n_sw/n_sw_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_sw, n_sw_by_right)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(n_sw, n_sw_by_right)$conf.int[2]) 

cite_request_loc_by_rights$software_right <- factor(cite_request_loc_by_rights$software_right, 
                                                     levels=c("Proprietary", "Non commercial", 
                                                              "Open source"))

cite_request_loc_by_rights$label <- factor(cite_request_loc_by_rights$label, 
                                           levels=c("Citation request available",
                                           "Citation request on webpage",
                                           "Citation request in repo README",
                                           "Citation metadata"))
cite_request_loc_by_rights %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(software_right)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/cite_request_loc_by_rights.png", width=6, height=4)


cite_request_format <- cite_request %>% 
  mutate(label=case_when(
    label=="plain text citation request" ~ "Plain text",
    label=="BibTex citation request" ~ "BibTex",
    label=="domain-specific citation request" ~ "Domain-specific",
    TRUE ~ as.character(label)
  )) %>% 
  filter(label %in% c("No citation request", "Plain text", 
                      "BibTex", "CITATION file", "CITATION.cff",
                      "CodeMeta", "Domain-specific")) %>% 
  filter(value==1) %>% 
  distinct(sample_id, label) 

cite_request_format_by_rights <- cite_request_format %>% 
  left_join(mentioned_rights, by="sample_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, software_right, label) %>% 
  group_by(software_right) %>% 
  mutate(n_sw_by_right=n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(label, software_right, n_sw_by_right) %>% 
  mutate(n_sw = n_distinct(software_name)) %>% 
  ungroup() %>% 
  distinct(label, software_right, n_sw, n_sw_by_right) %>% 
  filter(software_right != "Not accessible") %>% 
  mutate(proportion=n_sw/n_sw_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_sw, n_sw_by_right)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(n_sw, n_sw_by_right)$conf.int[2]) 

cite_request_format_by_rights$software_right <- factor(cite_request_format_by_rights$software_right, 
                                                    levels=c("Proprietary", "Non commercial", 
                                                             "Open source"))

cite_request_format_by_rights$label <- factor(cite_request_format_by_rights$label, 
                                           levels=c("No citation request",
                                                    "Plain text",
                                                    "BibTex",
                                                    "CITATION file",
                                                    "Domain-specific"))

cite_request_format_by_rights %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(software_right)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/cite_request_format_by_rights.png", width=6, height=4)


requested <- categories %>% 
  filter(coding_id %in% c("D10", "D11", "D12", "D13", "D14")) %>%
  select(-coding_scheme, -coding_id) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(`request to cite software`=as.numeric(`request to cite software`),
         `request to cite software publication`=as.numeric(`request to cite software publication`),
         `request to cite domain science publication`=as.numeric(`request to cite domain science publication`),
         `request to cite project`=as.numeric(`request to cite project`),
         `request to cite other research product`=as.numeric(`request to cite other research product`)) %>% 
  pivot_longer(cols=`request to cite software`:`request to cite other research product`, names_to="label", values_to="value") %>%  
  mutate(label=case_when(
    label=="request to cite software" ~ "Software",
    label=="request to cite software publication" ~ "Software publication",
    label=="request to cite domain science publication" ~ "Domain publication",
    label=="request to cite project" ~ "Project",
    label=="request to cite other research product" ~ "Other research product",
    TRUE ~ as.character(label)
  )) %>% 
  filter(value == 1) %>% 
  distinct(sample_id, label) 

requested_by_rights <- requested %>% 
  left_join(mentioned_rights, by="sample_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, label, software_right) %>% 
  group_by(software_right) %>% 
  mutate(n_sw_by_right=n_distinct(software_name)) %>% 
  ungroup() %>% 
  group_by(label, software_right, n_sw_by_right) %>% 
  summarise(n_sw = n_distinct(software_name)) %>% 
  ungroup() %>% 
  mutate(proportion=n_sw/n_sw_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_sw, n_sw_by_right)$conf.int[1]) %>% 
  mutate(conf_int_high=prop.test(n_sw, n_sw_by_right)$conf.int[2]) 

requested_by_rights$software_right <- factor(requested_by_rights$software_right, 
                                                       levels=c("Proprietary", "Non commercial", 
                                                                "Open source"))

requested_by_rights$label <- factor(requested_by_rights$label, 
                                              levels=c("Software",
                                                       "Software publication",
                                                       "Domain publication",
                                                       "Project",
                                                       "Other research product"))

requested_by_rights %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  facet_grid(rows=vars(software_right)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/new-figures-revision/request_by_rights.png", width=6, height=4)


metadata_by_rights <- categories %>% 
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
  distinct(sample_id, label, value) %>% 
  left_join(mentioned_rights, by="sample_id") %>% 
  left_join(name_list, by="sample_id") %>% 
  distinct(software_name, software_right, label, value) %>% 
  group_by(software_right) %>% 
  mutate(n_sw_by_right=n_distinct(software_name)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  group_by(label, software_right, n_sw_by_right) %>% 
  summarise(n_sw=n_distinct(software_name)) %>%
  ungroup() 

metadata_by_rights$software_right <- factor(metadata_by_rights$software_right, 
                                             levels=c("Proprietary", "Non commercial", 
                                                      "Open source"))

missed_rows <- tribble(
  ~label, ~software_right, ~n_sw_by_right, ~n_sw,
  "Archived", "Proprietary", 37, 0,
  "Unique and persistent identifier", "Proprietary", 37, 0
)

metadata_by_rights <- metadata_by_rights %>% bind_rows(missed_rows)

e <- metadata_by_rights %>% 
  mutate(n_sw_a = n_sw_by_right-n_sw) %>% 
  mutate(label = case_when(
    label=="Archived" ~ "Not archived",
    label=="Metadata available" ~ "No metadata",
    label=="Unique and persistent identifier" ~ "No unique and persistent identifier",
    TRUE ~ as.character("NA")
  )) %>% 
  distinct(label, software_right, n_sw_a, n_sw_by_right) %>% 
  rename(n_sw="n_sw_a") %>% 
  bind_rows(metadata_by_rights) %>% 
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
  arrange(software_right) %>% 
  mutate(proportion=n_sw/n_sw_by_right) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(n_sw, n_sw_by_right)$conf.int[1],
         conf_int_high=prop.test(n_sw, n_sw_by_right)$conf.int[2]) %>% 
  mutate(conf_int_low=if_else(value=="FALSE", 0, conf_int_low)) %>% 
  mutate(conf_int_high=if_else(value=="FALSE", 0, conf_int_high)) %>% 
  mutate(conf_int_high=if_else(value=="TRUE" & software_right=="Proprietary" & label!="Metadata available",
                               0, conf_int_high)) %>% 
  mutate(conf_int_low=na_if(conf_int_low, 0),
         conf_int_high=na_if(conf_int_high, 0)) 

e$software_right <- factor(e$software_right, levels=c("Proprietary", 
                                                      "Non commercial", "Open source"))

e$label <- factor(e$label, levels=c("Unique and persistent identifier", 
                                    "Metadata available", "Archived"))

e$value <- factor(e$value, levels=c("FALSE", "TRUE"))

e %>% 
  ggplot(aes(y=label, x=proportion, fill=value)) +
  geom_bar(stat='identity', position='fill') +
  geom_errorbar(aes(xmin=conf_int_low, xmax=conf_int_high), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("lightgray", "darkgray"), labels=c("FALSE", "TRUE"))+
  scale_y_discrete(name="") +
  scale_x_continuous(name="Proportion", limits=c(0,1)) +
  facet_grid(rows=vars(software_right)) +
  guides(fill=guide_legend(reverse=T)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        legend.position="bottom",
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="draft/output/standard_compliant_by_rights.png", width=6.5, height=4)
