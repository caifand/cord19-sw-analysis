# August 30, 2021. Mon
# Analyzing categories

library(tidyverse)
library(xtable)

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

james_a <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_james-coded.csv")
james_b <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.1_james.csv")
hannah <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_hannah-updated.csv")
fan <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_fan.csv")
agreement <- read_csv("cord19-sw-analysis/data/agreement_coding/agreement_coding.csv")

james_c <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.2_james-updated.csv")
hannah_b <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.2_hannah-updated.csv")
fan_b <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.2_fan-updated.csv")
fan_c <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.2_fan_2.csv")
fan_d <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.2_fan_3.csv")

all <- rbind(james_a, james_b, hannah, fan, agreement, james_c, hannah_b, 
             fan_b, fan_c, fan_d)
all %>% distinct(sample_id)
all %>% write_csv("data/full_coding_results.csv")

all <- read_csv("data/full_coding_results.csv")

# sanity check
all %>% 
  filter(coding_id=="A1" & coding_result==1) %>% 
  distinct(sample_id) %>% 
  mutate(sample_id=str_extract(sample_id, "\\d+-\\d+-\\d+")) %>% 
  left_join(frame, by="sample_id") %>% drop_na(anno_key) %>% 
  distinct(sample_id, anno_key, doc_key, group_num, doc_num, anno_num, issue_year) %>% 
  filter(issue_year > 2015) %>% 
  group_by(group_num) %>% 
  summarise(doc_count=n_distinct(doc_key)) %>% View

frame %>% 
  distinct(mention_density, doc_key) %>% 
  mutate(mention_density_group = cut(mention_density, breaks=c(0,1,8,350),
                                     labels=c("(0,1]","[2,8]","[9,350]"))) %>% 
  group_by(mention_density_group) %>% 
  summarise(doc_count=n_distinct(doc_key)) %>% 
  ggplot(aes(x=mention_density_group, y=doc_count)) + 
  geom_bar(stat='identity', fill='darkgray') +
  geom_text(aes(label=doc_count, x=mention_density_group, y=doc_count),
            position=position_dodge(width=0.4), vjust=-.8, size=3) +
  scale_x_discrete(name="Mention density per article") +
  scale_y_continuous(name="Number of articles mentioning software", limits=c(0,30000))
ggsave(filename="output/mention_density_group.png", width=6, height=4)

frame %>% 
  distinct(doc_key, mention_density_group, stratum) %>% 
  group_by(mention_density_group, stratum) %>% 
  summarise(doc_count=n_distinct(doc_key)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=stratum, values_from=doc_count) %>% 
  mutate(`[1,10]`=replace_na(`[1,10]`, 0)) %>% 
  rename(`Stratum
          Mention density`="mention_density_group") %>% 
  xtable(., type='latex')

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
all %>% group_by(sample_id) %>% 
  summarise(row_n = n()) %>% 
  arrange(desc(row_n)) %>% View

all_cleaned <- all_for_join %>% 
  filter(sample_id %in% all_valid_id) %>% 
  mutate(coding_result=replace_na(coding_result, 0)) %>%
  filter(!coding_id %in% c("A12", "A13", "B1", "B8", "D15", "D16", "E4")) 


# calculate false positive rate
false_pos <- all %>% 
  filter(coding_id == "A1") %>% 
  mutate(coding_result=replace_na(coding_result, 0)) %>%
  distinct(sample_id, coding_id, coding_result) %>% 
  group_by(coding_result) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% pull(mention_count)

conf_int <- prop.test(false_pos[1], false_pos[2])$conf.int
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
    coding_id == "D12" ~ "request to cite domain science publication",
    coding_id == "D13" ~ "request to cite project",
    coding_id == "D14" ~ "request to cite other research product",
    coding_id == "E1" ~ "software is archived",
    coding_id == "E2" ~ "software has unique, persistent identifier",
    coding_id == "E3" ~ "software has publicly accessible metadata",
    TRUE              ~ as.character(coding_id)
  )) 

# types of software mentions
mention_types <- categories %>% 
  filter(coding_id %in% c("A2", "A3", "A5", "A7", "A9", "A11",
                          "B3", "B4", "B5", "B6", "B7", "B9",
                          "B10", "B11", "B12", "C4", "C5")) %>% 
# note that 157 true positive software mentions here
  group_by(category, coding_result) %>% 
  summarise(mention_count = n_distinct(sample_id)) %>% 
  # ungroup() %>% 
  pivot_wider(names_from=coding_result,
              values_from=mention_count) %>% 
  rename(c(true="1", false="0")) %>% 
  mutate(false = replace_na(false, 0),
         true = replace_na(true, 0),
         sum = false + true) %>% 
  mutate(proportion = round(true/sum,3)) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(true,sum)$conf.int[1],
         conf_int_high=prop.test(true,sum)$conf.int[2])
# we don't have non-named software this time
# but this could be biased by the extraction?

# no mention cites software itself in this sample
# then no reference is a software reference. 
# The utility of them for detection would mostly be:
# if they are software publications, they provide names etc. of the software

# now plotting this group
# TODO: UpSet chart w/ name, version, publisher, URL, formal citation
# TODO: also calculate CI for this

# bar plot first: name only, like instrument, URL given, 
# informal mention, configuration details,
# cite to software publication, cite to domain publication

mention_type_plot <- categories %>% 
  distinct(sample_id, category, coding_result) %>% 
  pivot_wider(names_from=category, values_from=coding_result) %>% 
  mutate(label = case_when(
    `cite to software` == 1 ~ "Cite to software",
    `in-text name` == 1 & `in-text version` == 0 & 
      `in-text publisher` == 0 & `in-text URL` == 0  ~ "Name only",
    `like instrument` == 1 ~ "Like instrument",
    `in-text URL` == 1 ~ "URL in text",
    `cite to domain publication` == 1 ~ "Cite to domain publication",
    `cite to software publication` == 1 ~ "Cite to software publication",
    TRUE ~ as.character("NA")
  )) %>% 
  select(sample_id, label) %>% 
  group_by(label) %>% 
  summarise(mention_count = as.numeric(n_distinct(sample_id))) %>% 
  ungroup() %>% 
  filter(label != "NA") %>% 
  mutate(sum = 210) %>% 
  mutate(proportion = round(mention_count/sum, 3)) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, sum)$conf.int[1],
         conf_int_high=prop.test(mention_count, sum)$conf.int[2]) 


mention_type_plot$label <- factor(mention_type_plot$label, 
                                  levels=c("Cite to software",
                                           "Cite to domain publication",
                                           "Cite to software publication",
                                           "Like instrument",
                                           "URL in text",
                                           "Name only"))

mention_type_plot %>% 
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
ggsave(filename="output/mention_types.png", width=6, height=4)

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
  group_by(label) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  mutate(sum=sum(mention_count)) %>% 
  mutate(proportion = mention_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, sum)$conf.int[1],
         conf_int_high=prop.test(mention_count, sum)$conf.int[2])

condensed_categories$label <- factor(condensed_categories$label,
                                     levels=c("Cite to software",
                                              "Cite to publication",
                                              "Like instrument",
                                              "Informal"))
condensed_categories %>% 
  ggplot(aes(x=label, y=proportion)) +
  geom_bar(stat="identity", fill="darkgray") +
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
ggsave(filename="output/mention_types_condensed.png", width=6, height=4)

accessibility <- categories %>% 
  filter(category %in% c("no access", "proprietary",
                         "free access", "source code accessible", "modifiable")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(accessible=if_else(`no access`==0, 1, 0)) %>% 
  select(-`no access`) %>% 
  mutate(proprietary=as.numeric(proprietary),
         `free access`=as.numeric(`free access`),
         `source code accessible`=as.numeric(`source code accessible`),
         modifiable=as.numeric(modifiable)) %>%
  pivot_longer(!sample_id, names_to='label', values_to='value') %>% 
  mutate(label=case_when(
    label=="proprietary" ~ "accessible",
    label=="free access" ~ "free",
    TRUE ~ as.character(label)
  )) %>% 
  distinct() %>% 
  group_by(label, value) %>%
  summarise(mention_count = n_distinct(sample_id)) %>% 
  filter(value==1) %>% 
  mutate(sum=210) %>% 
  mutate(proportion = round(mention_count/sum,3)) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count,sum)$conf.int[1],
         conf_int_high=prop.test(mention_count,sum)$conf.int[2]) 

# now plotting citation functions
# access <- categories %>% 
#   filter(category %in% c("no access", "proprietary", "free access")) %>% 
#   select(-coding_id, -coding_scheme) %>% 
#   pivot_wider(names_from="category", values_from="coding_result") %>% 
#   mutate(category = case_when(
#     `no access` == 0 ~ "accessible",
#     `proprietary` == 1 ~ "accessible",
#     `free access` == 1 ~ "accessible",
#     TRUE ~ as.character("not accessible")
#   )) %>% 
#   select(-`no access`, -proprietary, -`free access`) %>% 
#   mutate(coding_result = if_else(category=="accessible", "1", "0")) %>% 
#   mutate(category ="accessible") %>% 
#   select(sample_id, coding_result, category) 
# 
# mention_functions <- categories %>% 
#   filter(category %in% c("identifiable", "findable", "source code accessible", 
#                          "modifiable")) %>% 
#   select(-coding_id, -coding_scheme) %>% 
#   bind_rows(access) %>% 
#   arrange(sample_id) %>% 
#   group_by(category, coding_result) %>% 
#   summarise(mention_count = n_distinct(sample_id)) %>% 
#   filter(coding_result == "1") %>% 
#   mutate(sum = 210) %>% 
#   mutate(proportion = mention_count/sum) %>% 
#   select(-coding_result) %>% 
#   rowwise() %>% 
#   mutate(conf_int_low=prop.test(mention_count,sum)$conf.int[1],
#          conf_int_high=prop.test(mention_count,sum)$conf.int[2])

accessibility$label <- factor(accessibility$label, 
                                     levels=c("accessible", "free", 
                                              "source code accessible",
                                              "modifiable"))

accessibility %>% 
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
ggsave(filename="output/accessibility.png", width=6, height=4)

mentioned_access <- categories %>% 
  filter(category %in% c("no access", "proprietary", "free access",
                         "source code accessible", "modifiable",
                         "open source licensed")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from=category, values_from=coding_result) %>% 
  rename(c(`not accessible`="no access",
           `open source`="open source licensed")) %>% 
  mutate(`non commercial`=if_else(`not accessible`=="0" & 
                                    `open source`=="0" & `proprietary`=="0", 
                                  "1", "0")) %>% 
  select(-`free access`, -`source code accessible`, -modifiable) %>% 
  pivot_longer(!sample_id, names_to="category", values_to="coding_result") %>% 
  filter(coding_result==1) %>% 
  select(-coding_result) %>% 
  group_by(category) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  mutate(sum = sum(mention_count)) %>% 
  mutate(proportion=mention_count/sum) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count,sum)$conf.int[1],
         conf_int_high=prop.test(mention_count,sum)$conf.int[2])

mentioned_access$category <- factor(mentioned_access$category,
                                    levels=c("not accessible", "proprietary",
                                            "non commercial", "open source"))

mentioned_access %>% 
  ggplot(aes(x=category, y=proportion)) +
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
ggsave(filename="output/mentioned_software_types.png", width=6, height=4)

# software access X mention type
a <- categories %>% 
  filter(category %in% c("no access", "proprietary", "free access",
                         "source code accessible", "modifiable",
                         "open source licensed")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from=category, values_from=coding_result) %>% 
  rename(c(`Not accessible`="no access",
           Proprietary="proprietary",
           `Open source`="open source licensed")) %>% 
  mutate(`on commercial`=if_else(`not accessible`=="0" & 
                                    `open source`=="0" & `proprietary`=="0", 
                                  "1", "0")) %>% 
  select(-`free access`, -`source code accessible`, -modifiable) %>% 
  pivot_longer(!sample_id, names_to="category", values_to="coding_result") %>% 
  filter(coding_result==1) %>% 
  select(-coding_result) 

b <- categories %>% 
  filter(category == "like instrument") %>% 
  bind_rows(categories %>% filter(category == "cite to domain publication")) %>% 
  bind_rows(categories %>% filter(category == "cite to software publication")) %>% 
  bind_rows(categories %>% filter(category == "cite to software")) %>% 
  bind_rows(categories %>% filter(category == "in-text name")) %>% 
  select(-coding_id, -coding_scheme) %>% 
  pivot_wider(names_from="category", values_from="coding_result") %>% 
  mutate(label = case_when(
    `cite to software` == 1 ~ "cite to software",
    `like instrument` == 1 ~ "like instrument",
    `cite to domain publication` == 1 ~ "cite to publication",
    `cite to software publication` == 1 ~ "cite to publication",
    `in-text name` == 1 ~ "informal",
    TRUE ~ as.character(sample_id),
  )) %>% 
  select(sample_id, label)

c<- a %>% 
  left_join(b, by="sample_id") %>%
  rename(c(software_type="category", mention_type="label")) %>% 
  group_by(software_type, mention_type) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  pivot_wider(names_from="mention_type", values_from="mention_count") %>% 
  mutate(`cite to publication`=replace_na(`cite to publication`,0),
         `cite to software`=replace_na(`cite to software`, 0)) 

sum(c[,2:5])
chisq.test(c[,2:5])

type_cite <- a %>% 
  left_join(b, by="sample_id") %>%
  rename(c(software_type="category", mention_type="label")) %>% 
  group_by(software_type, mention_type) %>% 
  summarise(mention_count=n_distinct(sample_id)) %>% 
  ungroup() %>% 
  group_by(software_type) %>% 
  mutate(mention_type_count=sum(mention_count)) %>% 
  mutate(proportion=mention_count/mention_type_count) %>% 
  rowwise() %>% 
  mutate(conf_int_low=prop.test(mention_count, mention_type_count)$conf.int[1],
         conf_int_high=prop.test(mention_count, mention_type_count)$conf.int[2]) 

type_cite$software_type <- factor(type_cite$software_type,
                          levels=c("not accessible", "proprietary",
                                   "non commercial", "open source"))
type_cite$mention_type <- factor(type_cite$mention_type,
                         levels=c("cite to software", "cite to publication",
                                  "like instrument", "informal"))

c %>% 
  ggplot(aes(x=mention_type, y=proportion)) +
  geom_bar(stat='identity', fill='darkgray') +
  facet_wrap(vars(software_type), nrow=1) +
  geom_errorbar(aes(ymin=conf_int_low, ymax=conf_int_high),width=.2,
                position=position_dodge(.9)) + 
  scale_x_discrete(name="") +
  scale_y_continuous(name="Proportion") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        text = element_text(size=10),
        axis.title.y = element_text(vjust=0.3),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(filename="output/software_x_mention_type.png", width=6.4, height=4)

