# August 30, 2021
# Consistency check

library(tidyverse)

james_a <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_james-coded.csv")
james_b <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1.1_james.csv")
hannah <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_hannah-updated.csv")
fan <- read_csv("cord19-sw-analysis/data/sample_coding/coding_sheet_v1_fan.csv")

# Consistency check for james_a
james_a <- james_a %>% 
  mutate(coding_result=replace_na(coding_result, "0")) 

james_a %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A1") %>% 
  filter(coding_result == "0") %>% View
# 4 false positives of software mention extractions

james_a %>% 
  filter(coding_id == "A3") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(coding_id == "A4") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# no false positive for extracted software name 

james_a %>% 
  filter(coding_id == "A5") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(coding_id == "A6") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# no false positive for extracted software version

james_a %>% 
  filter(coding_id == "A7") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(coding_id == "A8") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# no false positive for extracted software publisher

james_a %>% 
  filter(coding_id == "A9") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(coding_id == "A10") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# no false positive for extracted software URL

james_a %>% 
  filter(coding_id == "B1") %>% 
  filter(coding_result == "0") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# no software reference no coding in B section

james_a %>% 
  filter(coding_id == "B1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_results for software reference

james_a %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "B8") %>% View
# feel free to remove B8 for analysis

james_a %>% 
  filter(coding_id == "D1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_a, by="sample_id") %>% 
  filter(str_detect(coding_id, "D")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_result for citation request section

james_a %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D15") %>% View
# feel free to remove D15 for analysis

james_a %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D16") %>% View
# feel free to remove D16 for analysis

james_a %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D20") %>% View
# feel free to remove D20 for analysis


# Consistency check for james_b
james_b <- james_b %>% 
  mutate(coding_result=replace_na(coding_result, "0")) 

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A1") %>% 
  filter(coding_result == "0") %>% View
# no false positives of software mention extractions

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A3") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(coding_id == "A4") %>% 
  filter(coding_result == "0") %>% View
# no false positive for extracted software name 

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A5") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(coding_id == "A6") %>% 
  filter(coding_result == "0") %>% View
# no false positive for extracted software version

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A7") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(coding_id == "A8") %>% 
  filter(coding_result == "0") %>% View
# two false positive for extracted software publisher

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A9") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(coding_id == "A10") %>% 
  filter(coding_result == "0") %>% View
# no false positive for extracted software URL

james_b %>% 
  filter(coding_id == "B1") %>% 
  filter(coding_result == "0") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# seems like we have a problematic reference extraction

james_b %>%
  filter(coding_id == "B1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>%  
  distinct(sample_id, coding_id, coding_result) %>% View
# no reference spotted in this section

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "B8") %>% View
# feel free to remove B8 for analysis

james_b %>% 
  filter(coding_id == "D1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(james_b, by="sample_id") %>% 
  filter(str_detect(coding_id, "D")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_result for citation request section

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D15") %>% View
# feel free to remove D15 for analysis

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D16") %>% View
# feel free to remove D16 for analysis

james_b %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D20") %>% View
# feel free to remove D20 for analysis


# Consistency check for hannah
hannah <- hannah %>% 
  mutate(coding_result=replace_na(coding_result, "0")) 

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A1") %>% 
  filter(coding_result == "0") %>% View
# one false positives of software mention extractions

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A3") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(coding_id == "A4") %>% 
  filter(coding_result == "0") %>% View
# 3 problematic software name extraction spotted

# addtional check
hannah %>%
  filter(coding_id == "A3") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(coding_id == "A4") %>% 
  filter(coding_result == "0") %>%
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_result for mention extraction with problematic names

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A5") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(coding_id == "A6") %>% 
  filter(coding_result == "0") %>% View
# no false positive for extracted software version

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A7") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(coding_id == "A8") %>% 
  filter(coding_result == "0") %>% View
# four false positive for extracted software publisher

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A9") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(coding_id == "A10") %>% 
  filter(coding_result == "0") %>% View
# no false positive for extracted software URL

hannah %>% 
  filter(coding_id == "B1") %>% 
  filter(coding_result == "0") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# seems like we have four problematic reference extraction

hannah %>%
  filter(coding_id == "B1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>%  
  distinct(sample_id, coding_id, coding_result) %>% View
# no reference spotted in this section

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "B8") %>% View
# feel free to remove B8 for analysis

hannah %>% 
  filter(coding_id == "D1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(hannah, by="sample_id") %>% 
  filter(str_detect(coding_id, "D")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_result for citation request section

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D15") %>% View
# feel free to remove D15 for analysis

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D16") %>% View
# feel free to remove D16 for analysis

hannah %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D20") %>% View
# feel free to remove D20 for analysis


# Consistency check for fan
fan <- fan %>% 
  mutate(coding_result=replace_na(coding_result, "0")) 

fan %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "A1") %>% 
  filter(coding_result == "0") %>% View
# two false positives of software mention extractions

fan %>% 
  filter(coding_id == "A3") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(coding_id == "A4") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# two problematic software name extraction spotted


fan %>% 
  filter(coding_id == "A5") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(coding_id == "A6") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# one false positive for extracted software version

fan %>% 
  filter(coding_id == "A7") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(coding_id == "A8") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# one false positive for extracted software publisher

fan %>% 
  filter(coding_id == "A9") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(coding_id == "A10") %>% 
  filter(coding_result == "0") %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# one false positive for extracted software URL

fan %>% 
  filter(coding_id == "B1") %>% 
  filter(coding_result == "0") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# generally looks good

fan %>%
  filter(coding_id == "B1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(str_detect(coding_id, "B")) %>%  
  distinct(sample_id, coding_id, coding_result) %>% View
# no reference spotted in this section

fan %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "B8") %>% View
# one problematic reference extraction spotted
# feel free to remove B8 for analysis

fan %>% 
  filter(coding_id == "D1") %>% 
  filter(coding_result == "1") %>% 
  select(sample_id) %>% 
  left_join(fan, by="sample_id") %>% 
  filter(str_detect(coding_id, "D")) %>% 
  distinct(sample_id, coding_id, coding_result) %>% View
# valid coding_result for citation request section

fan %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D15") %>% View
# feel free to remove D15 for analysis

fan %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D16") %>% View
# feel free to remove D16 for analysis

fan %>% 
  distinct(sample_id, coding_id, coding_result) %>% 
  filter(coding_id == "D20") %>% View
# feel free to remove D20 for analysis