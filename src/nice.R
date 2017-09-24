# --------------------------------------------------------------------------- #
# Treaty of Nice
# --------------------------------------------------------------------------- #

nice = read_html("https://en.wikisource.org/wiki/Treaty_of_Nice")

treaty = nice %>%
  html_nodes("p,dl,h2,h3") %>%
  html_text()

nice = data.frame(text = treaty)

nice$text = str_replace_all(nice$text, "\n", "")
nice = unique(nice)

nice$index = seq_len(nrow(nice))

# table of contents --------------------------------------------------------- #

nice_toc = get_toc("https://en.wikisource.org/wiki/Treaty_of_Nice")

# only titles and chapters
nice_toc = nice_toc %>% 
  separate(tocnumber, into = c("part"), sep = "\\.", remove = FALSE)

nice_toc = filter(nice_toc, tocnumber %in% c(1,2,3,4))
nice_toc$part = as.numeric(nice_toc$part) - 1

# delete [edit]
nice$text = str_replace_all(nice$text, "\\[edit\\]", " ")

nice_toc = nice_toc[,c("toctext", "part")]

names(nice_toc)[names(nice_toc)=="toctext"] <- "text"

# trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
nice_toc$text = trim(nice_toc$text)
nice$text = trim(nice$text)

# merge
nice = merge(nice, nice_toc, by = "text", all = TRUE)

# sort 
nice = nice[order(nice$index),]

nice = nice %>% fill(part)

rm(nice_toc, treaty)

# get articles -------------------------------------------------------------- #

nice$article = as.numeric(unlist(str_extract_all(str_extract_all(nice$text, "^Article \\d{1,}"), "\\d{1,}")))
nice$article[nice$article == 0] = NA
nice$article[nice$article > 13] = NA

nice = nice[order(nice$index),]

nice = subset(nice, nice$index >= 67 & nice$index <= 544)
nice = subset(nice, nice$index != 529)

# fix and fill articles ----------------------------------------------------- #

nice$article[nice$index == 190] = NA

nice = nice %>% fill(article)

# concatenate text ---------------------------------------------------------- #

nice = nice[order(nice$index),]

nice = nice %>%
  group_by(part, article) %>%
  summarize(txt = paste(text, collapse = " "))

nice = nice[order(nice$article),]

# create id variable -------------------------------------------------------- #

nice$treaty = 8

nice = nice %>% 
  unite(id, c("treaty", "part", "article"), sep = ".", remove = FALSE)

nice$id = str_replace_all(nice$id, "NA", "X")

# save ---------------------------------------------------------------------- #

write_csv(nice, "tables/nice.csv")


