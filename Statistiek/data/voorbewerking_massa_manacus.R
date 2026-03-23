# Script om grote dataset van dieren in Colombia te filteren zodat het alleen
# data over de Bonte manakin (Manacus manacus) bevat.

library(readr)
library(tidyverse)

# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3273

massa <- read_delim("data/ecy3273-sup-0001-datas1/DataS1_Body mass records_DOetal.csv", 
                    delim = ";", 
                    # Voorkom dat de weight column verkeerd wordt omgezet
                    col_types = cols(`Weight (gr)` = col_character()), 
                    trim_ws = TRUE)

# Komma's vervangen door punten zodat de kolommen numeriek kunnen zijn
massa$`Weight (gr)` <- as.numeric(gsub(",", ".", massa$`Weight (gr)`))
massa$ReferenceW <- as.numeric(gsub(",", ".", massa$ReferenceW))
massa$`DifferenceW-RefW` <- as.numeric(gsub(",", ".", massa$`DifferenceW-RefW`))

massa_manacus <- massa %>% 
  filter(Species == "Manacus manacus") %>%
  select(RecordType, Sex, `Weight (gr)`, ReferenceW, `DifferenceW-RefW`, Latitude, Logitude)

colnames(massa_manacus)[3] <- "Weight"

write.csv(massa_manacus, file = "data/massa_manacus.csv", row.names = FALSE)
