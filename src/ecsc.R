# --------------------------------------------------------------------------- #
# The Treaty establishing the European Coal and Steel Community (ECSC)
# --------------------------------------------------------------------------- #

ecsc = read_html("https://en.wikisource.org/wiki/The_Treaty_establishing_the_European_Coal_and_Steel_Community_(ECSC)")

treaty = ecsc %>%
  html_nodes("p,li,h2,h3") %>%
  html_text()

ecsc = data.frame(text = treaty)
rm(treaty)

ecsc$index = seq_len(nrow(ecsc))

# table of contents --------------------------------------------------------- #

ecsc_toc = get_toc("https://en.wikisource.org/wiki/The_Treaty_establishing_the_European_Coal_and_Steel_Community_(ECSC)")

# only titles and chapters
ecsc_toc = ecsc_toc %>% 
  separate(tocnumber, into = c("title", "chapter"), sep = "\\.", remove = FALSE)

# delete [edit]
ecsc$text = str_replace_all(ecsc$text, "\\[edit\\]", " ")

# select columns
ecsc_toc = ecsc_toc[,c("toctext", "title", "chapter")]

names(ecsc_toc)[names(ecsc_toc)=="toctext"] = "text"

# trim whitespace
ecsc_toc$text = trim(ecsc_toc$text)
ecsc$text = trim(ecsc$text)

# merge
ecsc = merge(ecsc, ecsc_toc, by = "text", all = TRUE)
rm(ecsc_toc)

# sort 
ecsc = ecsc[order(ecsc$index),]

ecsc = ecsc %>% fill(title)
ecsc = ecsc %>% group_by(title) %>% fill(chapter)

# get articles -------------------------------------------------------------- #

ecsc$article = NA
ecsc$article = as.numeric(unlist(str_extract_all(str_extract_all(ecsc$text, "^Article \\d{1,}"), "\\d{1,}")))
ecsc$article[ecsc$article == 0] = NA

ecsc = ecsc[order(ecsc$index),]

ecsc = ecsc %>% fill(article)

# remove titles and chapters ------------------------------------------------ #

ecsc$remove_1 = str_extract_all(ecsc$text, "^TITLE")
ecsc$remove_2 = str_extract_all(ecsc$text, "^CHAPTER")
ecsc = subset(ecsc, ecsc$remove_1 != "TITLE")
ecsc = subset(ecsc, ecsc$remove_2 != "CHAPTER")
ecsc = subset(ecsc, select = c("text", "index", "title", "chapter", "article"))

# concatenate text ---------------------------------------------------------- #

ecsc = ecsc[order(ecsc$index),]
ecsc = subset(ecsc, ecsc$index >= 39 & ecsc$index <= 479)

ecsc = ecsc %>%
  group_by(title, chapter, article) %>%
  summarize(txt = paste(text, collapse = " "))

ecsc = ecsc[order(ecsc$article),]

# create id variable -------------------------------------------------------- #

ecsc$treaty = 1

ecsc = ecsc %>% 
  unite(id, c("treaty", "title", "chapter", "article"), sep = ".", remove = FALSE)

ecsc$id = str_replace_all(ecsc$id, "NA", "X")

# save individual ----------------------------------------------------------- #

write_csv(ecsc, "tables/ecsc.csv")

# save ---------------------------------------------------------------------- #

# first treaty
ecsc = subset(ecsc, select = c("id", "txt"))

names(ecsc)[names(ecsc)=="id"] = "id_1951"

saveRDS(ecsc, "data/eulaw_1951.rds")

# --------------------------------------------------------------------------- #
