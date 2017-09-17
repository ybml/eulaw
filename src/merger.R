# --------------------------------------------------------------------------- #
# Merger Treaty
# --------------------------------------------------------------------------- #

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
  summarize(txt = paste(text, collapse = " "))

merger = merger[order(merger$article),]

# create id variable -------------------------------------------------------- #

merger$treaty = 4

merger = merger %>% 
  unite(id, c("treaty", "chapter", "article"), sep = ".", remove = FALSE)

merger$id = str_replace_all(merger$id, "NA", "X")

# save
write_csv(merger, "tables/merger.csv")

# make a change file manually ----------------------------------------------- #

# go through every article and collect changes: 
# "add", 
# "repeal", "repeal_txt", 
# "replace", "replace_txt", 
# "insert"
# mark changes to a (for now) irrelevant document with "remnant"


# Apply changes ------------------------------------------------------------- #
# Remove all variables except the functions.
rm(list = setdiff(ls(), lsf.str()))

# Load merger_changes and eulaw_1957 files.
merger_changes <- read_csv("tables/merger_changes.csv")
eulaw_1957 <- readRDS("data/eulaw_1957.rds")

# Pre-processing on the changes file.
merger_changes <- set_new_txt(merger_changes, "add") %>%
  set_action(old_action = "add", new_action = "insert") %>%
  set_new_id(., "insert", id_field = id) %>%
  set_new_id(., "replace", change_id)

# Apply the changes.
eulaw_1965 <- apply_changes(eulaw_1957, merger_changes, "1965")

# Save ---------------------------------------------------------------------- #
saveRDS(eulaw_1965, "data/eulaw_1965.rds")


## # FRIE ------------------------------------------------------------------- #

## # 1. read data

## # 1.1. read back in merger
## merger <- read_csv("data/merger.csv", col_types = "ccccc")
## merger$article <- str_replace_all(merger$article, "\\.0", "") # delete .0 from article number

## # download google sheet data
## # dl_ws("merger") # only run to update changes in googledocs to local files 

## # 1.2. changes data
## merger_changes <- read_csv("data/merger_changes.csv", col_types = "ccccccc")
## merger_orig <- read_csv("data/merger_orig.csv", col_types = "cc")
## # no global changes in merger 

## # 1.3. old data 
## t57 <- read_rds("data/1957_2.rds")

## # 2. Some data cleaning
## # 2.1. prepare changes data frame 
## merger_changes <- merger_changes %>% 
##   mutate(new_txt = if_else(is.na(new_txt) & !is.na(old_txt), "", new_txt)) %>% 
##   fill(treaty) %>% 
##   fill(merger_change_id)

## # join the stupid id 
## merger_changes <- left_join(merger_changes %>% select(article = merger_change_id, everything()),
##                             merger %>% select(article, merger_change_id = merger_id),
##                             by = "article")
## merger_changes$article <- NULL

## # 2.2. prepare original articles 
## merger_sub <- merger %>% 
##   ungroup() %>% 
##   select(article, text, ends_with("id"))

## merger_orig <- left_join(merger_orig, merger_sub, by = "article")
## merger_orig$current_id <- merger_orig$merger_id 
## merger_orig$article <- NULL # delete article variable


## # 3. Apply Changes
## # 3.1. Changes
## # join merger_changes
## t65 <- full_join(t57, merger_changes, by = c("treaty", "current_id"))

## # apply changes
## t65 <- apply_changes(t65, merger_id)

## # 3.2. Original articles 
## # add original articles
## t65 <- bind_rows(t65, merger_orig)


## # save ------------------------------------------------------------ #
## write_rds(t65, "data/1965.rds")
