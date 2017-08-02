# --------------------------------------------------------------------------- #
# The Treaty establishing the European Coal and Steel Community (ECSC)
# --------------------------------------------------------------------------- #

# August 2, 2017
ecsc = read_html("https://en.wikisource.org/wiki/The_Treaty_establishing_the_European_Coal_and_Steel_Community_(ECSC)")

treaty = ecsc %>%
  html_nodes("p,li,h2,h3") %>%
  html_text()

ecsc = data.frame(text = treaty)

ecsc$index = seq_len(nrow(ecsc))

# table of contents --------------------------------------------------------- #

ecsc_toc = get_toc("https://en.wikisource.org/wiki/The_Treaty_establishing_the_European_Coal_and_Steel_Community_(ECSC)")

# only titles and chapters
ecsc_toc = ecsc_toc %>% 
  separate(tocnumber, into = c("title", "chapter"), sep = "\\.", remove = FALSE)

# delete [edit]
ecsc$text = str_replace_all(ecsc$text, "\\[edit\\]", " ")

ecsc_toc = ecsc_toc[,c("toctext", "title", "chapter")]

names(ecsc_toc)[names(ecsc_toc)=="toctext"] <- "text"

# trim whitespace
trim = function (x) gsub("^\\s+|\\s+$", "", x)
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
  summarize(text = paste(text, collapse = " "))

ecsc = ecsc[order(ecsc$article),]

write.csv(ecsc, file = "data/ecsc.csv")

# create id variable -------------------------------------------------------- #

ecsc = ecsc %>% 
  unite(ecsc_id, title:article, sep = ".")

ecsc$ecsc_id = str_replace_all(ecsc$ecsc_id, "NA", "X")
ecsc$treaty = "ecsc"

ecsc = ecsc %>% 
  select(treaty, ecsc_id, text)

# save ---------------------------------------------------------------------- #
saveRDS(ecsc, "data/1951.rds")