# August 24, 2021. Tue
# Check agreement

library(tidyverse)
library(irr)

fan <- read_csv("data/coding_sheet_for_agreement_fan.csv")
james <- read_csv("data/coding_sheet_for_agreement_james.csv")

exclude_codes <- c("B8", "D15", "D16")

fan_coding <- fan %>% 
  filter(!coding_id %in% exclude_codes) %>% 
  pull(coding_result)

james_coding <- james %>% 
  filter(!coding_id %in% exclude_codes) %>% 
  pull(coding_result)
james_coding[is.na(james_coding)] <- 0
james_coding[james_coding == "NNA"] <- 0

codings <- tibble(fan_coding, james_coding)
agree(codings)
# percentage agreement 93.3

