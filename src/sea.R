# --------------------------------------------------------------------------- #
# Single European Act
# --------------------------------------------------------------------------- #

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

sea = subset(sea, select = c("text", "index", "title", "chapter", "section", "subsection", "article"))

# concatenate text ---------------------------------------------------------- #

sea = sea[order(sea$index),] # check
sea = subset(sea, sea$index >= 62 & sea$index <= 343)

sea = sea %>%
  group_by(title, chapter, section, subsection, article) %>%
  summarize(txt = paste(text, collapse = " "))

sea = sea[order(sea$article),]

# create id variable -------------------------------------------------------- #

sea$treaty = 5

sea = sea %>% 
  unite(id, c("treaty", "title", "chapter", "section", "subsection", "article"), sep = ".", remove = FALSE)

sea$id = str_replace_all(sea$id, "NA", "X")

# changes -------------------------------------------------------------------#

write_csv(sea, "tables/sea.csv")

# load change file
sea = read_csv("tables/sea_changes.csv")




# FRIE -------------------------------------------------------------------#

# find ids for eec
# sapply(c(7, 49, 54, 56, 57, 57, 149, 237, 238, 145, "None", 188, rep("None", 3), 28, 57, 59, 70, 84, 99), lookup_id, treaty_df = eec, idvar = eec_id) %>% write_clip()

# 1. read data

# 1.1. read back in sea
sea <- read_csv("data/sea.csv")
sea <- sea %>% 
  mutate_all(funs(as.character))

# download google sheet data
# gs_auth() # run to get access to your sheets
# dl_ws("sea") # only run if you have access

# 1.2. changes data
sea_changes <- read_csv("data/sea_changes.csv", col_types = "ccccccc")
sea_orig <- read_csv("data/sea_orig.csv", col_types = "cc")
# no global changes in sea 

# 1.3. read in data from previous change round
t65 <- read_rds("data/1965.rds")

# 2. some data cleaning
# 2.1. prepare changes data frame 
sea_changes <- sea_changes %>% 
  mutate(new_txt = if_else(is.na(new_txt) & !is.na(old_txt), "", new_txt)) %>% 
  fill(treaty) %>% 
  fill(sea_change_id)

# join id from sea (to replace sea_change_id)
# NOTE: easier to hand code directly imho
sea_changes <- sea_changes %>% 
  select(article = sea_change_id, everything()) %>% # rename 
  left_join(sea %>% select(article, sea_change_id = sea_id), # rename 
                         by = "article") %>% 
  select(-article) # drop article column

# 2.2. prepare original articles
# again, add id (this time sea_id)
sea_orig <- left_join(sea_orig,
                      sea %>% select(sea_id, article, text), 
                      by  = "article")
sea_orig$current_id <- sea_orig$sea_id
sea_orig$article <- NULL

# 3. Apply Changes
# 3.1. Manual changes 

# change the affected chapter ids according to Article 20.2 SEA
# "2. Chapters 1, 2 and 3 shall become Chapters 2, 3 and 4 respectively."
# get the old ids for the affected cases 
old_id <- t65 %>% 
  filter(treaty == "eec" & str_detect(current_id, "^3\\.2\\.") == TRUE) %>% 
  pull(current_id)

# extract the chapter id which is at the 5th position of the string and add 1 
new_chapters <- as.integer(str_sub(old_id, 5, 5)) + 1


new_id <- paste0(str_sub(old_id, 1, 4), 
                 new_chapters,
                 str_sub(old_id, 6))

# replace in original data frame
t65$current_id[t65$treaty == "eec" & t65$current_id %in% old_id] <- new_id


# 3.2. Changes

# test whether all current_ids can be joined 
tmp <- anti_join(sea_changes, t65, by = c("treaty", "current_id"))
stopifnot(all(tmp$current_id == "None")) # <- those are the "add"ers, they 
# naturally do not have a matching id
rm(tmp)

# join sea_changes to t65
t86 <- full_join(t65, sea_changes, by = c("treaty", "current_id"))

# apply changes
t86 <- apply_changes(t86, sea_id)

# 3.3. Add original articles 

# bind together
t86 <- bind_rows(t86, sea_orig)

# save ------------------------------------------------------------ #
write_rds(t86, "data/1986.rds")


