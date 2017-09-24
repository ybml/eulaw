# Table of equivalence preparation

library(tidyverse)
library(DT)
library(stringr)

toe <- read_excel("tables/amsterdam_table_of_equivalences.xlsx", sheet = "eec")

toe <- toe %>%
  mutate(
    change_article_nr = str_extract(change_article, pattern = "(?<=e )\\d.*$"),
    new_article_nr = str_extract(new_article, pattern = "(?<=e )\\d.*$")
  )

articles <- toe %>%
  select(change_article, change_article_nr, new_article, new_article_nr) %>%
  filter(!is.na(change_article_nr) & !is.na(new_article_nr)) %>%
  pull(change_article_nr) %>%
  as.character()
articles

ids <- lapply(articles, function(i){lookup_id(eec_1997, id, i)}) %>%
  unlist()
ids <- c(ids[1:187], NA_character_, ids[188:274], NA_character_, ids[275:312])

toe <- toe %>%
  filter(!is.na(change_article_nr) & !is.na(new_article_nr)) %>%
  mutate(change_id = ids) %>%
  select(1:8, article_nr = new_article_nr, "new_id", "remarks")

write_csv(toe, path = "data/toe_eec.csv")

toe <- read_excel("tables/amsterdam_table_of_equivalences.xlsx", sheet = "teu")

toe <- toe %>%
  mutate(
    change_article_nr = str_extract(change_article, pattern = "(?<=e ).*$")
  )
ids <- lapply(pull(toe, change_article_nr),
              function(i){lookup_id(teu_1997, id, i)}) %>%
  unlist()
ids <- c(ids[1:7], NA_character_, ids[8:48], NA_character_, ids[49:51])

toe <- toe %>%
  mutate(change_id = ids,
         new_id = paste(treaty_nr, new_title_nr, new_article_nr, sep = ".")
  ) %>%
  select(-section_flag, -contains("nr"))

  
