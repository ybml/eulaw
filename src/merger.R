# --------------------------------------------------------------------------- #
# Merger Treaty
# --------------------------------------------------------------------------- #

# August 2, 2017
merger = read_html("https://en.wikisource.org/wiki/Merger_Treaty")

treaty = merger %>%
  html_nodes("h2,h3,p,dd") %>%
  html_text()

merger = data.frame(text = treaty)

merger$index = seq_len(nrow(merger))

# table of contents
merger_toc = get_toc("https://en.wikisource.org/wiki/Merger_Treaty")

# chapters and articles
merger_toc = merger_toc %>% 
  separate(tocnumber, into = c("chapter", "article"), sep = "\\.", remove = FALSE)

# delete [edit]
merger$text = str_replace_all(merger$text, "\\[edit\\]", " ")

names(merger_toc)[names(merger_toc)=="toctext"] <- "text"

merger_toc = merger_toc[,c("text", "chapter")]

# make new article variable
merger_toc$article = as.numeric(unlist(str_extract_all(str_extract_all(merger_toc$text, "^Article \\d{1,}"), "\\d{1,}")))
merger_toc$article[merger_toc$article == 0] = NA

# trim whitespace
merger_toc$text = trim(merger_toc$text)
merger$text = trim(merger$text)

# merge
merger = merge(merger, merger_toc, by = "text", all = TRUE)
rm(merger_toc)

# sort 
merger = merger[order(merger$index),]

# fill chapter numbers
merger = merger %>% fill(chapter)
# fill articles
merger = merger %>% fill(article)

# remove chapters 
merger$remove = str_extract_all(merger$text, "^CHAPTER")
merger = subset(merger, merger$remove != "CHAPTER")
merger = subset(merger, select = c("text", "index", "chapter", "article"))

# concatenate text
merger = merger[order(merger$index),]
merger = subset(merger, merger$index >= 26 & merger$index <= 202)

merger = merger %>%
  group_by(chapter, article) %>%
  summarize(text = paste(text, collapse = " "))

merger = merger[order(merger$article),]

write.csv(merger, file = "data/merger.csv")

# create id variable -------------------------------------------------------- #

merger = merger %>% 
  unite(merger_id, chapter:article, sep = ".")

merger$merger_id = str_replace_all(merger$merger_id, "NA", "X")
merger$treaty = "merger"

merger = merger %>% 
  select(treaty, merger_id, text)

# changes ------------------------------------------------------------------- #

# read data
merger_changes <- read_csv("data/changes_merger.csv")
t57 <- read_rds("data/1957_2.rds")

# prepare changes data frame 
merger_changes <- merger_changes %>% 
  select(treaty, current_id, action, old_txt, new_txt, merger_id = new_id) %>% 
  mutate(new_txt = if_else(is.na(new_txt) & !is.na(old_txt), "", new_txt)) %>% 
  fill(treaty)

# join merger_changes
t65 <- full_join(t57, merger_changes, by = c("treaty", "current_id"))

# apply and save
t65_2 <- apply_changes(t65, "merger")

# apply and save ------------------------------------------------------------ #
saveRDS(to_be_changed, "data/1965.rds")




