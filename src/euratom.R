# --------------------------------------------------------------------------- #
# Treaty establishing the European Atomic Energy Community (EURATOM)
# --------------------------------------------------------------------------- #

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

# fix section numbers (section 1, 2 and 3 in title 6 are not subordinate to a chapter)
euratom_toc$section[euratom_toc$title == 6 & euratom_toc$chapter == 1] = 1
euratom_toc$section[euratom_toc$title == 6 & euratom_toc$chapter == 2] = 2
euratom_toc$section[euratom_toc$title == 6 & euratom_toc$chapter == 3] = 3

euratom_toc$chapter[euratom_toc$title == 6 & euratom_toc$section == 1] = NA
euratom_toc$chapter[euratom_toc$title == 6 & euratom_toc$section == 2] = NA
euratom_toc$chapter[euratom_toc$title == 6 & euratom_toc$section == 3] = NA

# delete [edit]
euratom$text = str_replace_all(euratom$text, "\\[edit\\]", " ")

euratom_toc = euratom_toc[,c("toctext", "title", "chapter", "section")]

names(euratom_toc)[names(euratom_toc)=="toctext"] = "text"

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
  summarize(txt = paste(text, collapse = " "))

euratom = euratom[order(euratom$article),]

# create id variable -------------------------------------------------------- #

euratom$treaty = 2

euratom = euratom %>% 
  unite(id, c("treaty", "title", "chapter", "section", "article"), sep = ".", remove = FALSE)

euratom$id = str_replace_all(euratom$id, "NA", "X")

# save individual ----------------------------------------------------------- #

write_csv(euratom, "tables/euratom.csv")

# add to ecsc -------------------------------------------------------- #

# read in ecsc
eulaw = readRDS("data/eulaw_1951.rds")

# rbind (new articles only, no changes)
eulaw = bind_rows(eulaw, euratom)

# add id
eulaw = eulaw %>% 
  mutate(id = if_else(is.na(id), id_1951, id))

names(eulaw)[names(eulaw)=="id"] = "id_1957"

# save ---------------------------------------------------------------------- #

# keep all ids
eulaw = subset(eulaw, select = c("id_1951", "id_1957", "txt"))

saveRDS(eulaw, "data/eulaw_1957.rds")

# --------------------------------------------------------------------------- #

