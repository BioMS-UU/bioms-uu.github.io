library(readr)
library(tidyverse)

# Dataset met lichaamsgewicht van vogels en zoogdieren in Colombia
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

# Kijk welke soorten er zijn en hoe veel observaties elke soort heeft
table(massa$Species)

massa %>%
  group_by(Species, Family, Order) %>% 
  filter(n() > 100) %>% # Retain species with more than 50 occurrences
  ggplot(aes(x = `Weight (gr)`)) +
  geom_histogram() +
  facet_wrap(vars(Species), scales = "free")

# Voorbeeld met de vogel Manacus manacus
massa %>% 
  filter(Species == "Manacus manacus") %>%
  select(`Weight (gr)`) %>%
  as.vector() %>%
  unlist() %>%
  qqnorm(., main = "Normal")

massa_manacus <- massa %>% filter(Species == "Manacus manacus")

ggplot(massa_manacus, aes(x = `Weight (gr)`)) +
 # geom_histogram(binwidth = 1) +
  geom_density(color = "darkorange", fill = "darkorange", alpha = 0.5) +
  theme_minimal() +
  xlab("Gewicht (g)") +
  ylab("Aantal observaties")

# Wat is het gemiddelde gewicht van de vogels?
t.test(massa_manacus$`Weight (gr)`)

massa_manacus %>%
  filter(Sex != "Undetermined") %>%
  ggplot(aes(x = `Weight (gr)`, fill = Sex, color = Sex)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    xlab("Gewicht (g)") +
    ylab("Kans") +
    scale_fill_discrete(name = "Geslacht",
                        labels = c("Vrouwelijk", "Mannelijk")) +
    guides(color = "none")

# Is er een verschil in het gewicht tussen mannetjes en vrouwtjes?
t.test(massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Male"],
       massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Female"])

summary(massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Male"])
summary(massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Female"])
sd(massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Male"])
sd(massa_manacus$`Weight (gr)`[massa_manacus$Sex == "Female"])
library("car")
leveneTest(`Weight` ~ Sex, data = massa_manacus)

