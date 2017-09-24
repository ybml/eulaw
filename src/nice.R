# --------------------------------------------------------------------------- #
# Treaty of Nice
# --------------------------------------------------------------------------- #

# Clean the workspace
rm(list = ls())
source("src/utils.R")

# Get html.
h <- read_html("https://en.wikisource.org/wiki/Treaty_of_Nice")
xp <- "//dl//dd/dl//dd|//p|//dl//dd[not(dl)]|//h3//span[@class='mw-headline']"

# Extract source
text <- h %>%
  html_nodes(xpath = xp) %>%
  html_text()

# Clean extracted text ------------------------------------------------------ #
# Remove the part "FINAL ACT"
text <- tibble(text,
              helper = if_else(str_detect(text, "The CONF"), 1, NA_real_)
       ) %>%
  fill(helper) %>%
  filter(is.na(helper)) %>%
  pull(text)

# Remove the note that the signatures were ommitted.
text <- text[!str_detect(text, "\\[")]

# Replace all linefeeds with a space.
text <- str_replace_all(text, pattern = "\\n", " ")

# Tibble one row one article, cols article number and text ------------------ #

ptrn = "(^Article \\d?:|^Article \\d{1,2}$)"
nice <- data_frame(
          art_ind = str_detect(text, ptrn),
          txt = text
        )

# Generate article number and remove article titles.
nice <-  nice %>%
  mutate(
    art_nr = if_else(art_ind,
                     str_extract(txt, "(?<=Article )\\d{1,2}"),
                     NA_character_
             ),
    
    ) %>%
  fill(art_nr) %>%
  filter(!art_ind & !is.na(art_nr))

# One row one article
nice <- nice %>%
  group_by(art_nr) %>%
  summarise(art_txt = paste0(txt, collapse = " ")) %>%
  ungroup() %>%
  select(art_nr, art_txt) %>%
  mutate(art_nr = as.numeric(art_nr)) %>%
  arrange(art_nr)


# Generate id --------------------------------------------------------------- #
nice <- nice %>%
  mutate(part_nr = if_else(art_nr < 7, 1, 2),
         id = paste("8", part_nr, art_nr, sep = ".")
  ) %>%
  select(id, part_nr, art_nr, txt = art_txt)

write_csv(nice, path = "tables/nice.csv")
## nice = data.frame(text = treaty)

## nice$text = str_replace_all(nice$text, "\n", " ")
## nice = unique(nice)

## nice$index = seq_len(nrow(nice))

## # table of contents --------------------------------------------------------- #

## nice_toc = get_toc("https://en.wikisource.org/wiki/Treaty_of_Nice")

## # only titles and chapters
## nice_toc = nice_toc %>% 
##   separate(tocnumber, into = c("part"), sep = "\\.", remove = FALSE)

## nice_toc = filter(nice_toc, tocnumber %in% c(1,2,3,4))
## nice_toc$part = as.numeric(nice_toc$part) - 1

## # delete [edit]
## nice$text = str_replace_all(nice$text, "\\[edit\\]", " ")

## nice_toc = nice_toc[,c("toctext", "part")]

## names(nice_toc)[names(nice_toc)=="toctext"] <- "text"

## # trim whitespace
## trim <- function (x) gsub("^\\s+|\\s+$", "", x)
## nice_toc$text = trim(nice_toc$text)
## nice$text = trim(nice$text)

## # merge
## nice = merge(nice, nice_toc, by = "text", all = TRUE)

## # sort 
## nice = nice[order(nice$index),]

## nice = nice %>% fill(part)

## rm(nice_toc, treaty)

## # get articles -------------------------------------------------------------- #

## nice$article = as.numeric(unlist(str_extract_all(str_extract_all(nice$text, "^Article \\d{1,}"), "\\d{1,}")))
## nice$article[nice$article == 0] = NA
## nice$article[nice$article > 13] = NA

## nice = nice[order(nice$index),]

## nice = subset(nice, nice$index >= 67 & nice$index <= 544)
## nice = subset(nice, nice$index != 529)

## # fix and fill articles ----------------------------------------------------- #

## nice$article[nice$index == 190] = NA

## nice = nice %>% fill(article)

## # concatenate text ---------------------------------------------------------- #

## nice = nice[order(nice$index),]

## nice = nice %>%
##   group_by(part, article) %>%
##   summarize(txt = paste(text, collapse = " "))

## nice = nice[order(nice$article),]

## # create id variable -------------------------------------------------------- #

## nice$treaty = 8

## nice = nice %>% 
##   unite(id, c("treaty", "part", "article"), sep = ".", remove = FALSE)

## nice$id = str_replace_all(nice$id, "NA", "X")

## # save ---------------------------------------------------------------------- #

## write_csv(nice, "tables/nice.csv")
