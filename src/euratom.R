# --------------------------------------------------------------------------- #
# Treaty establishing the European Atomic Energy Community (EURATOM)
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
euratom = read_html("https://en.wikisource.org/wiki/Treaty_establishing_the_European_Atomic_Energy_Community")

treaty = euratom %>%
  html_nodes("h2,h3,h4,p,li,dt,dd") %>%
  html_text()

euratom = data.frame(text = treaty)
rm(treaty)

euratom$index = seq_len(nrow(euratom))

# table of contents --------------------------------------------------------- #

euratom_toc = get_toc("https://en.wikisource.org/wiki/Treaty_establishing_the_European_Atomic_Energy_Community")

# only titles and chapters
euratom_toc = euratom_toc %>% 
  separate(tocnumber, into = c("title", "chapter", "section"), sep = "\\.", remove = FALSE)

# delete [edit]
euratom$text = str_replace_all(euratom$text, "\\[edit\\]", " ")

euratom_toc = euratom_toc[,c("toctext", "title", "chapter", "section")]

names(euratom_toc)[names(euratom_toc)=="toctext"] <- "text"

# trim whitespace
euratom_toc$text = trim(euratom_toc$text)
euratom$text = trim(euratom$text)

# merge
euratom = merge(euratom, euratom_toc, by = "text", all = TRUE)
rm(euratom_toc)

# sort 
euratom = euratom[order(euratom$index),]

euratom = euratom %>% fill(title)
euratom = euratom %>% group_by(title) %>% fill(chapter)
euratom = euratom %>% group_by(title, chapter) %>% fill(section)

# get articles -------------------------------------------------------------- #

euratom$article = as.numeric(unlist(str_extract_all(str_extract_all(euratom$text, "^Article \\d{1,}"), "\\d{1,}")))
euratom$article[euratom$article == 0] = NA

euratom = euratom[order(euratom$index),]

euratom = euratom %>% fill(article)

# remove titles and chapters ------------------------------------------------ #

euratom$remove_1 = str_extract_all(euratom$text, "^TITLE")
euratom$remove_2 = str_extract_all(euratom$text, "^CHAPTER")
euratom$remove_3 = str_extract_all(euratom$text, "^Section")

euratom = subset(euratom, euratom$remove_1 != "TITLE")
euratom = subset(euratom, euratom$remove_2 != "CHAPTER")
euratom = subset(euratom, euratom$remove_3 != "Section")
euratom = subset(euratom, euratom$text != "Final Provisions")

# remove sub-sections that are not in toc
euratom = subset(euratom, euratom$text != "(a) Dissemination by amicable arrangement")
euratom = subset(euratom, euratom$text != "(b) Ex officio communication to the Commission")
euratom = subset(euratom, euratom$text != "(c) Licences granted by means of arbitration or ex officio")

euratom = subset(euratom, select = c("text", "index", "title", "chapter", "section", "article"))

# concatenate text ---------------------------------------------------------- #

euratom = euratom[order(euratom$index),]
euratom = subset(euratom, euratom$index >= 58 & euratom$index <= 986)

euratom = euratom %>%
  group_by(title, chapter, section, article) %>%
  summarize(text = paste(text, collapse = " "))

euratom = euratom[order(euratom$article),]


# create id variable -------------------------------------------------------- #

euratom = euratom %>% 
  unite(euratom_id, title:article, sep = ".", remove = FALSE)

euratom$euratom_id = str_replace_all(euratom$euratom_id, "NA", "X")
euratom$treaty = "euratom"

write_csv(euratom, "data/euratom.csv")

# add to ecsc -------------------------------------------------------- #

euratom = euratom %>% 
  ungroup() %>% 
  select(treaty, euratom_id, text)

# read in ecsc
ecsc <- readRDS("data/1951.rds")

ecsc_euratom <- bind_rows(ecsc, euratom)

# add ecsc ids to euratom_id
ecsc_euratom <- ecsc_euratom %>% 
  mutate(current_id = if_else(is.na(euratom_id), ecsc_id, euratom_id))

# save ---------------------------------------------------------------------- #
saveRDS(ecsc_euratom, "data/1957_1.rds")

rm(euratom, ecsc, ecsc_euratom)
