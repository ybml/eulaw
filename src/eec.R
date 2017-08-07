# --------------------------------------------------------------------------- #
# Treaty establishing a European Economic Community (EEC)
# --------------------------------------------------------------------------- #

# load packages
require(rvest)
require(purrr)
require(purrrlyr)
require(stringr)
require(tidyr)
require(dplyr)
require(readr)

# August 2, 2017
eec = read_html("https://en.wikisource.org/wiki/Treaty_establishing_the_European_Economic_Community")

treaty = eec %>%
  html_nodes("h2,h3,h4,h5,p,li,dt,dd") %>%
  html_text()

eec = data.frame(text = treaty)
rm(treaty)

eec$index = seq_len(nrow(eec))

# table of contents --------------------------------------------------------- #

eec_toc = get_toc("https://en.wikisource.org/wiki/Treaty_establishing_the_European_Economic_Community")

# only titles and chapters
eec_toc = eec_toc %>% 
  separate(tocnumber, into = c("part", "title", "chapter", "section"), sep = "\\.", remove = FALSE)

# delete [edit]
eec$text = str_replace_all(eec$text, "\\[edit\\]", " ")

eec_toc = eec_toc[,c("toctext", "part", "title", "chapter", "section")]

names(eec_toc)[names(eec_toc)=="toctext"] <- "text"

# trim whitespace
eec_toc$text = trim(eec_toc$text)
eec$text = trim(eec$text)

# merge
eec = merge(eec, eec_toc, by = "text", all = TRUE)
rm(eec_toc)

# sort 
eec = eec[order(eec$index),]

eec = eec %>% fill(part)
eec = eec %>% group_by(part) %>% fill(title)
eec = eec %>% group_by(part, title) %>% fill(chapter)
eec = eec %>% group_by(part, title, chapter) %>% fill(section)

# get articles -------------------------------------------------------------- #

eec$article = as.numeric(unlist(str_extract_all(str_extract_all(eec$text, "^Article \\d{1,}"), "\\d{1,}")))
eec$article[eec$article == 0] = NA

eec = eec[order(eec$index),]

eec = eec %>% fill(article)

# remove titles and chapters ------------------------------------------------ #

eec$remove_1 = str_extract_all(eec$text, "^PART")
eec$remove_2 = str_extract_all(eec$text, "^TITLE")
eec$remove_3 = str_extract_all(eec$text, "^Chapter")
eec$remove_4 = str_extract_all(eec$text, "^Section")

eec = subset(eec, eec$remove_1 != "PART")
eec = subset(eec, eec$remove_2 != "TITLE")
eec = subset(eec, eec$remove_3 != "Chapter")
eec = subset(eec, eec$remove_4 != "Section")

eec = subset(eec, eec$text != "Final Provisions")

eec = subset(eec, select = c("text", "index", "part", "title", "chapter", "section", "article"))

# concatenate text ---------------------------------------------------------- #

eec = eec[order(eec$index),]
eec = subset(eec, eec$index >= 70 & eec$index <= 1157)

eec = eec %>%
  group_by(part, title, chapter, section, article) %>%
  summarize(text = paste(text, collapse = " "))

eec = eec[order(eec$article),]



# create id variable -------------------------------------------------------- #

eec = eec %>% 
  unite(eec_id, part:article, sep = ".", remove = FALSE)

eec$eec_id = str_replace_all(eec$eec_id, "NA", "X")
eec$treaty = "eec"

write_csv(eec, "data/eec.csv")

# add to previous ----------------------------------------------------------- #

eec = eec %>% 
  ungroup() %>% 
  select(treaty, eec_id, text)

# read 
ecsc_euratom <- readRDS("data/1957_1.rds")

ecsc_euratom_eec <- bind_rows(ecsc_euratom, eec)
ecsc_euratom_eec <- ecsc_euratom_eec %>% 
  mutate(current_id = if_else(is.na(current_id), eec_id, current_id))

# save ---------------------------------------------------------------------- #
saveRDS(ecsc_euratom_eec, "data/1957_2.rds")

rm(eec, ecsc_euratom, ecsc_euratom_eec)