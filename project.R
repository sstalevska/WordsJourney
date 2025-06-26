# project.R
library(dplyr)
library(readr)

# Load datasets
words_relations <- read_csv("rstudio/Infoviz Proekt/archive/words_relations.csv")
etymology <- read_csv("rstudio/Infoviz Proekt/archive/etymology.csv")
dict <- read_csv("rstudio/Infoviz Proekt/archive/dict.csv")
semantic_displacement <- read_csv("rstudio/Infoviz Proekt/archive/semantic_displacement.csv")


# Normalize column names
names(dict) <- tolower(names(dict))
names(etymology) <- tolower(names(etymology))

# Save full datasets
saveRDS(dict, "dict.rds")
saveRDS(etymology, "etymology.rds")
saveRDS(words_relations, "relations.rds")
saveRDS(semantic_displacement, "semantic_displacement.rds")


# Get top 20 source languages (borrowed origins)
top_languages <- etymology %>%
  filter(!is.na(related_lang)) %>%
  count(related_lang, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  pull(related_lang)

saveRDS(top_languages, "top_languages.rds")

# For the "roots" dropdown, use most common source roots
top_roots <- etymology %>%
  filter(!is.na(related_lang)) %>%
  count(related_lang, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  pull(related_lang)

saveRDS(top_roots, "top_roots.rds")
