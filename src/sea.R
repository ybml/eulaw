# --------------------------------------------------------------------------- #
# Single European Act
# --------------------------------------------------------------------------- #

# August 6, 2017
sea = read_html("https://en.wikisource.org/wiki/Single_European_Act")

treaty = sea %>%
  html_nodes("h2,h3,h4,h5,p") %>%
  html_text()

sea = data.frame(text = treaty)

sea$index = seq_len(nrow(sea))

# table of contents
sea_toc = get_toc("https://en.wikisource.org/wiki/Single_European_Act")

# chapters and articles
sea_toc = sea_toc %>% 
  separate(tocnumber, into = c("title", "chapter", "section", "subsection"), sep = "\\.", remove = FALSE)

# fix title numbering
sea_toc$title = as.character(as.numeric(sea_toc$title) - 1)

# delete [edit]
sea$text = str_replace_all(sea$text, "\\[edit\\]", " ")

names(sea_toc)[names(sea_toc)=="toctext"] <- "text"

sea_toc = sea_toc[,c("text", "title", "chapter", "section", "subsection")]

# trim whitespace
sea_toc$text = trim(sea_toc$text)
sea$text = trim(sea$text)

# merge
sea = merge(sea, sea_toc, by = "text", all = TRUE)
rm(sea_toc)

# sort 
sea = sea[order(sea$index),] # check 

sea = sea %>% fill(title)
sea = sea %>% group_by(title) %>% fill(chapter)
sea = sea %>% group_by(title, chapter) %>% fill(section)
sea = sea %>% group_by(title, chapter, section) %>% fill(subsection)

# get articles -------------------------------------------------------------- #

sea$article = as.numeric(unlist(str_extract_all(str_extract_all(sea$text, "^Article \\d{1,}"), "\\d{1,}")))
sea$article[sea$article == 0] = NA

# fix article numbers
sea$article[sea$article > 40] = NA

sea = sea[order(sea$index),]

sea = sea %>% fill(article)

# remove titles and chapters ------------------------------------------------ #

sea$remove_1 = str_extract_all(sea$text, "^TITLE")
sea$remove_2 = str_extract_all(sea$text, "^CHAPTER")
sea$remove_3 = str_extract_all(sea$text, "^Section")
sea$remove_4 = str_extract_all(sea$text, "^Sub-section")

sea = subset(sea, sea$remove_1 != "TITLE")
sea = subset(sea, sea$remove_2 != "CHAPTER")
sea = subset(sea, sea$remove_3 != "Section")
sea = subset(sea, sea$remove_4 != "Sub-section")

sea = subset(sea, select = c("text", "index", "title", "chapter", "section", "subsection","article"))

# concatenate text ---------------------------------------------------------- #

sea = sea[order(sea$index),] # check
sea = subset(sea, sea$index >= 62 & sea$index <= 343)

sea = sea %>%
  group_by(title, chapter, section, subsection, article) %>%
  summarize(text = paste(text, collapse = " "))

sea = sea[order(sea$article),]


# create id variable -------------------------------------------------------- #

sea = sea %>% 
  unite(sea_id, title:article, sep = ".", remove = FALSE)

sea$sea_id = str_replace_all(sea$sea_id, "NA", "X")
sea$treaty = "sea"

write_csv(sea, "data/sea.csv")

sea = sea %>% 
  select(treaty, sea_id, text)

# changes -------------------------------------------------------------------#

# read data

# read back in sea
sea <- read_csv("data/sea.csv")
sea <- sea %>% 
  mutate_all(funs(as.character))

# download google sheet data
dl_ws("sea")

# changes
sea_changes <- read_csv("data/sea_changes.csv", col_types = "ccccccc")
sea_orig <- read_csv("data/sea_orig.csv", col_types = "cc")

# no global changes in sea 

t57 <- read_rds("data/1957_2.rds")

# prepare changes data frame 
sea_changes <- sea_changes %>% 
  mutate(new_txt = if_else(is.na(new_txt) & !is.na(old_txt), "", new_txt)) %>% 
  fill(treaty) %>% 
  fill(sea_change_id)

# join sea_changes
t65 <- full_join(t57, sea_changes, by = c("treaty", "current_id"))

# apply changes
t65 <- apply_changes(t65, sea_id)

# add original articles
t65 <- add_orig(sea_orig, sea, t65)

# apply and save ------------------------------------------------------------ #
write_rds(t65, "data/1965.rds")



