# --------------------------------------------------------------------------- #
# Treaty establishing a European Economic Community (EEC)
# --------------------------------------------------------------------------- #

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

names(eec_toc)[names(eec_toc)=="toctext"] = "text"

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
  summarize(txt = paste(text, collapse = " "))

eec = eec[order(eec$article),]

# create id variable -------------------------------------------------------- #

eec$treaty = 3

eec = eec %>% 
  unite(id, c("treaty", "part", "title", "chapter", "section", "article"), sep = ".", remove = FALSE)

eec$id = str_replace_all(eec$id, "NA", "X")

# save individual ----------------------------------------------------------- #

write_csv(eec, "tables/eec.csv")

# add to previous ----------------------------------------------------------- #

# read 
eulaw = readRDS("data/eulaw_1957.rds")

# rbind
eulaw = bind_rows(eulaw, eec)

# add id
eulaw = eulaw %>% 
  mutate(id = if_else(is.na(id), id_1957, id))

# keep only one id_1957
eulaw = subset(eulaw, select = c("id_1951", "id", "txt"))

names(eulaw)[names(eulaw)=="id"] = "id_1957"

# save ---------------------------------------------------------------------- #

saveRDS(eulaw, "data/eulaw_1957.rds")

# --------------------------------------------------------------------------- #
